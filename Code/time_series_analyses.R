### Time Series Analysis ###

# Purpose: Final analytical code for replicating all analyses in the paper.

# Libraries
library(CausalImpact)
library(data.table)
library(lubridate)
library(forecast)
library(magrittr)
library(ggplot2)
library(tsModel)
library(lmtest)
library(tidyr)
library(zoo)

### Part 1: Setting up data for analysis ###

# Note not replicable due to data sharing agreement and our ethical approval. Skip to Section 2 for code to replicate time series analyses. Code below included for full open sharing of all code.

# Load Twitter data
lockdown <- read.csv("./Data/twitter_covid_lockdown_mar2020_small_cleaned.csv", header = TRUE)
backup <- lockdown
lockdown <- backup
lockdown <- lockdown[,c("id", "user_id", "created_at", "quote_count", "reply_count", "retweet_count", "favorite_count", "full_text_cleaned")] # Keep only variables required for analysis (saves processing time later)

# Load misinformation lookups
lkup_misinfo <- read.csv("./Data/lookup_all_misinformation.csv") # All tweets discussing misinformation we have identified
lkup_misinfo$X <- NULL # Tidy

lkup_snowball <- read.csv("./Data/lookup_snowballed_misinformation.csv") # Subsequent retweets and quoted retweets from those above
lkup_snowball$X <- NULL # Tidy

lkup <- rbind(lkup_misinfo, lkup_snowball) # Combine each
lkup <- distinct(lkup, id, .keep_all = TRUE) # Remove duplicate tweets
rm(lkup_misinfo, lkup_snowball) # Tidy

# Load estimated probability user is a bot
bots <- read.csv("./Data/predict_bots_lockdown.csv")
bots <- bots[,c("user_id", "prob_bot")] # Subset required measures
bots$bot[bots$prob_bot >= 0.5] <- 1 # Define bots as probabiluty >= 0.5
bots$bot[bots$prob_bot < 0.5] <- 0

# Join on misinformation lookup onto twitter data file 
lkup$id <- as.factor(lkup$id) # To match dataset to help joining process
lockdown <- merge(lockdown, lkup, by.x = "id", by.y = "id", all.x = TRUE) # Join
lockdown$misinformation[is.na(lockdown$misinformation)] <- 0 # If is missing a value, then give a 0
lockdown$not_true_not_false[is.na(lockdown$not_true_not_false)] <- 0
lockdown$false[is.na(lockdown$false)] <- 0
lockdown$cause[is.na(lockdown$cause)] <- 0
lockdown$transmission[is.na(lockdown$transmission)] <- 0
lockdown$treatment[is.na(lockdown$treatment)] <- 0

# Join bots lookup on
lockdown <- merge(lockdown, bots, by.x = "user_id", by.y = "user_id", all.x = TRUE) # Join
lockdown$prob_bot <- NULL

# Edit time variable to consistent format for R
lockdown$created_at <- as.POSIXct(lockdown$created_at, format = "%a %b %d %H:%M:%S %z %Y", tz = "GMT") 
lockdown$day <- cut(lockdown$created_at, breaks="day") # Split by day
lockdown$hour <- cut(lockdown$created_at, breaks="hour") # Split by day
lockdown$minute <- cut(lockdown$created_at, breaks="min") # Split by minute
#lockdown$30mins <- cut(lockdown$created_at, breaks="30 mins") # Split by 30 minutes
#lockdown$second <- cut(lockdown$created_at, breaks="sec") # Split by minute

# Aggregate data by time period to creat time-series data format
#day_avg <- aggregate(misinformation~day, lockdown, sum) # Aggregate count of fake news per day
#day_avg$day <- as.Date(day_avg$day, format = "%Y-%m-%d %H:%M:%S") # Convert to date (as factor)
lockdown$tweets <- 1 # Generic variable for counting tweets
dt <- data.table(lockdown)
# Day
tw_day <- dt[, list(misinformation = sum(misinformation, na.rm = TRUE), tweets = sum(tweets, na.rm = TRUE), bots = sum(bot, na.rm = TRUE)), by = "day"] # Aggregate by day
# Hour
tw_hour <- dt[, list(misinformation = sum(misinformation, na.rm = TRUE), tweets = sum(tweets, na.rm = TRUE), bots = sum(bot, na.rm = TRUE)), by = "hour"] # Aggregate by day
tw_hour <- tw_hour[!is.na(tw_hour$hour)] # Remove missing data
tw_hour$hour <- ymd_hms(tw_hour$hour) # Convert to date-time format
tw_hour_long <- gather(tw_hour, type, count, misinformation:bots, factor_key=TRUE) # Convert to long format if want to plot multiple tweet types at once

# Repeat process for each minute
tw_minute <- dt[, list(misinformation = sum(misinformation, na.rm = TRUE), tweets = sum(tweets, na.rm = TRUE), bots = sum(bot, na.rm = TRUE)), by = "minute"]
tw_minute <- tw_minute[!is.na(tw_minute$minute)] 
tw_minute$minute <- ymd_hms(tw_minute$minute) 
tw_minute_long <- gather(tw_minute, type, count, misinformation:tweets, factor_key=TRUE) 

# Define intervention period
# Hours
tw_hour$intervention[tw_hour$hour < "2020-03-23 20:00:00"] <- 0
tw_hour$intervention[tw_hour$hour >= "2020-03-23 20:00:00"] <- 1
# Minutes
tw_minute$intervention[tw_minute$minute < "2020-03-23 20:00:00"] <- 0
tw_minute$intervention[tw_minute$minute >= "2020-03-23 20:00:00"] <- 1
# Raw
lockdown$created_at <- ymd_hms(lockdown$created_at) 
lockdown$intervention[lockdown$created_at < "2020-03-23 20:00:00"] <- 0
lockdown$intervention[lockdown$created_at >= "2020-03-23 20:00:00"] <- 1

# Save data
write.csv(tw_hour, "./Data/tweets_per_hour.csv") 
write.csv(tw_hour_long, "./Data/tweets_per_hour_long.csv") 
write.csv(tw_minute, "./Data/tweets_per_min.csv")
write.csv(tw_minute_long, "./Data/tweets_per_min_long.csv")
write.csv(lockdown, "./Data/lockdown_misinformation_merge.csv")


### Part 2: Descriptive Analyses ###


# Load data
tw_hour <- read.csv("./Data/tweets_per_hour.csv")
tw_hour$hour <- ymd_hms(tw_hour$hour) 
tw_hour_long <- read.csv("./Data/tweets_per_hour_long.csv")
tw_hour_long$hour <- ymd_hms(tw_hour_long$hour) 
tw_minute <- read.csv("./Data/tweets_per_min.csv")
tw_minute$minute <- ymd_hms(tw_minute$minute) 
tw_minute_long <- read.csv("./Data/tweets_per_min_long.csv")
tw_minute_long$hour <- ymd_hms(tw_minute_long$hour) 

# Counts of outcome variables pre- and post-announcement
table(lockdown$tweets, lockdown$intervention)
table(lockdown$misinformation, lockdown$intervention)
table(lockdown$bot, lockdown$intervention)

# Number of tweets
sum(tw_hour$tweets)

# Bots by day
tw_day$bots_pc <- (tw_day$bots / tw_day$tweets)*100
tw_day

# Retweets
# Convert to numeric as factors
lockdown$retweet_count <- as.numeric(as.character(lockdown$retweet_count))
lockdown$reply_count <- as.numeric(as.character(lockdown$reply_count))
lockdown$quote_count <- as.numeric(as.character(lockdown$quote_count))
lockdown$favorite_count <- as.numeric(as.character(lockdown$favorite_count))
lockdown$engagement <- lockdown$quote_count + lockdown$reply_count + lockdown$retweet_count + lockdown$favorite_count

dtl <- data.table(lockdown)
dtl[, list(engagement = mean(engagement, na.rm = TRUE)), by = "bot"] # Mean by bot or not
t.test(lockdown$engagement~lockdown$bot) # t-test

dtl[, list(engagement = mean(engagement, na.rm = TRUE)), by = "misinformation"] # Mean by misinformation or not
t.test(lockdown$engagement~lockdown$misinformation)

dtl[, list(engagement = mean(engagement, na.rm = TRUE)), by = "intervention"] # Mean before and after announcement
t.test(lockdown$engagement~lockdown$intervention)


### Part 3: Visualisation of trends ###

# Plot number of tweets per hour
ggplot(tw_hour, aes(x = hour, y = tweets)) +
  geom_line() +
  #geom_smooth(method = "gam", se = F) +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  ylim(0,60000) +
  ylab("Number of tweets") +
  xlab("Date")

# Plot number of tweets per minute
ggplot(tw_minute, aes(x = minute, y = tweets)) +
  #geom_point() + 
  geom_line(alpha = 0.3) +
  #geom_smooth(method = "gam", se = F) +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  #ylim(0,60000) +
  ylab("Number of tweets") +
  xlab("Date")

# Plot number of misinformation tweets per minute
ggplot(tw_minute, aes(x = hour, y = misinformation)) +
  #geom_point() + 
  geom_line(alpha = 0.3) +
  #geom_smooth(method = "gam", se = F) +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  #ylim(0,60000) +
  ylab("Number of tweets") +
  xlab("Date")

# Plot in long format
# Hours
ggplot(tw_hour_long, aes(x = hour, y = count, group = type, color = type)) +
  geom_point() + # Consider just geom_path()
  #geom_smooth(method = "gam", se = F) +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  scale_y_continuous(trans = 'log2') + # Log transform y-axis
  ylab("Number of tweets") +
  xlab("Time")

# Minutes
ggplot(tw_minute_long, aes(x = hour, y = count, group = type, color = type)) +
  geom_point() + # Consider just geom_path()
  #geom_smooth(method = "gam", se = F) +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  scale_y_continuous(trans = 'log2') + # Log transform y-axis
  ylab("Number of tweets") +
  xlab("Time")

# Plot in facet format
# Specify order of facet plot
tw_hour_long$type <- factor(tw_hour_long$type, levels = c("misinformation", "tweets", "bots"))

# Hours
ggplot(tw_hour_long, aes(x = hour, y = count)) +
  geom_line() + # Consider just geom_path()
  facet_grid(vars(type), scales = "free") + # Remove scales = "free" if want to have same axis
  #geom_smooth(method = "gam", se = F) +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  #ylim(0,125) +
  ylab("Number of tweets") +
  xlab("Time")

ggsave(plot = last_plot(), filename = "./Plots/Figure_1.tiff", dpi = 300)

# Minutes
ggplot(tw_minute_long, aes(x = hour, y = count)) +
  geom_line() + # Consider just geom_path()
  facet_grid(vars(type), scales = "free") + # Remove scales = "free" if want to have same axis
  #geom_smooth(method = "gam", se = F) +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  #ylim(0,125) +
  ylab("Number of tweets") +
  xlab("Time")


### Part 4: Model interrupted time series ###


# Method 1: Causal Impact Model (Bayesian Structural Time Series and Counterfactual Estimation)

# For hours 

# Define intervention period
pre.period <- ymd_hms(c("2020-03-21 20:00:00", "2020-03-23 19:00:00")) # Periods must not overlap
post.period <- ymd_hms(c("2020-03-23 20:00:00", "2020-03-25 20:00:00"))

# Tidy data
for_model <- zoo(cbind(tw_hour$misinformation, tw_hour$tweets), tw_hour$hour) # Left cbind in here if want to add control variables
colnames(for_model) = c("y", "x1") # Rename columns (if more than one use c("y", "x1", etc) if only one then "y")

# Run analysis
model1a <- CausalImpact(for_model, pre.period, post.period) # Model
plot(model1a) # Plot results
summary(model1a) # Get summary statistics
# plot(model1$model$bsts.model, "coefficients") # If have lots of variables, can visualise which were included in the final model

# For bots - hours
for_model <- zoo(cbind(tw_hour$bots, tw_hour$tweets), tw_hour$hour)
colnames(for_model) = c("y", "x1")
model1c <- CausalImpact(for_model, pre.period, post.period) # Model
plot(model1c) # Plot results
summary(model1c) # Get summary statistics

# Repeat for minutes
pre.period <- ymd_hms(c("2020-03-21 20:00:00", "2020-03-23 19:59:00")) # Periods must not overlap
post.period <- ymd_hms(c("2020-03-23 20:00:00", "2020-03-25 20:00:00"))
for_model <- zoo(cbind(tw_minute$misinformation, tw_minute$tweets), tw_minute$minute)
colnames(for_model) = c("y", "x1")
model1b <- CausalImpact(for_model, pre.period, post.period) # Model
plot(model1b) # Plot results
summary(model1b) # Get summary statistics

# For bots - minutes
for_model <- zoo(cbind(tw_minute$bots, tw_minute$tweets), tw_minute$minute)
colnames(for_model) = c("y", "x1")
model1d <- CausalImpact(for_model, pre.period, post.period) # Model
plot(model1d) # Plot results
summary(model1d) # Get summary statistics


# Method 2: Poisson Regression Model

source("./Scripts/negativebinomial_modelling.R") # Run this for all analyses

# Method 3: ARIMA (time-series regression)

# Misinformation - hours data
preds <- cbind(tw_hour$tweets, tw_hour$intervention) # Define predictor variables
colnames(preds) <- c("tweets", "intervention")
model3 <- auto.arima(tw_hour$misinformation, xreg = preds) # Fit best fitting time series model
model3 # Print model
coeftest(model3) # Grab coefficients and model summary
confint(model3) # Confidence Intervals
checkresiduals(model3) # Check for auto-correlation

# Misinformation - minutes data
preds <- cbind(tw_minute$tweets, tw_minute$intervention) # Define predictor variables
colnames(preds) <- c("tweets", "intervention")
model3 <- auto.arima(tw_minute$misinformation, xreg = preds) # Fit best fitting time series model
model3 # Print model
coeftest(model3) # Grab coefficients and model summary
confint(model3) # Confidence Intervals
checkresiduals(model3) # Check for auto-correlation

# Bots - hours
preds <- cbind(tw_hour$tweets, tw_hour$intervention) # Define predictor variables
colnames(preds) <- c("tweets", "intervention")
model3 <- auto.arima(tw_hour$bots, xreg = preds) # Fit best fitting time series model
model3 # Print model
coeftest(model3) # Grab coefficients and model summary
confint(model3) # Confidence Intervals
checkresiduals(model3) # Check for auto-correlation

# Bots - minutes
preds <- cbind(tw_minute$tweets, tw_minute$intervention) # Define predictor variables
colnames(preds) <- c("tweets", "intervention")
model3 <- auto.arima(tw_minute$bots, xreg = preds) # Fit best fitting time series model
model3 # Print model
coeftest(model3) # Grab coefficients and model summary
confint(model3) # Confidence Intervals
checkresiduals(model3) # Check for auto-correlation


# # Method 4: Change point regression (Not included in paper)
# 
# ## First define potential breakpoints via data driven approach
# 
# # Create numeric value to represent time trend so easier to fit in packages
# tw_hour$trend <- as.numeric(tw_hour$hour - tw_hour$hour[1], units = "days")
# 
# # Estimate range of breakpoints (k = 8)
# bp <- breakpoints(tweets ~ trend, data = tw_hour, h = 3, breaks = 8) # Adjust h as minimum segment size via looking at data descriptively
# plot(bp) # Plot and select best solutions - want both BIC and RSS to be flat
# bp # ALso gives optimal model
# coef(bp, breaks = 3) # Display intercept and slope of all breakpoints
# tw_hour$day[breakpoints(bp, breaks = 3)$breakpoints] # Define breakpoints in date format
# confint(bp, breaks = 3)
# 
# ## Second, test them within a segmented regression model
# 
# # Fit a regression model (maybe need to update to specify correct model type)
# linear_model <- lm(tweets ~ trend, data = tw_hour)
# 
# # Test break points
# seg_model <- segmented(linear_model, seg.Z = ~ trend, psi = list(trend = c(21, 25, 29))) # psi defined based on above results
# summary(seg_model) # Model results
# slope(seg_model) # Estimate slopes
# 
# # Plot results
# my.fitted <- as.data.frame(fitted(seg_model)) # Predicted line values
# names(my.fitted)[names(my.fitted) == "fitted(seg_model)"] <- "pred" # Rename variable
# my.fitted <- tibble::rownames_to_column(my.fitted, "trend") # Create column of x values
# # my.fitted <- rbind(my.fitted, c(0,0)) # Add on row for 0 trend being value 0
# my.fitted$trend <- as.numeric(my.fitted$trend) # Convert data to numeric form
# my.fitted$trend <- my.fitted$trend - 1 # So matches original data
# 
# ggplot(my.fitted, aes(x = trend, y = pred)) + geom_line() # Plot segmented model
# ggplot(tw_hour, aes(x = trend, y = tweets)) + geom_line() + geom_line(data = my.fitted, aes(x = trend, y = pred), colour = "tomato") # Plot segmented model on top of actual data


### Part 5: Interrupted time series of topic models ###


# Load data
topics_long <- read.csv("./Data/topics_per_hour.csv")
topics_long$hour <- ymd_hms(topics_long$hour) 

# Calculate offset (count of all misinformation tweets - if need)
dt <- data.table(topics_long)
dt <- dt[, list(misinformation = sum(n, na.rm=TRUE)), by = "hour"] # Aggregate to get total
topics_long <- merge(topics_long, dt, by = "hour", all.x = TRUE) # Join back on

# Define intervention
topics_long$intervention[topics_long$hour < "2020-03-23 20:00:00"] <- 0
topics_long$intervention[topics_long$hour >= "2020-03-23 20:00:00"] <- 1

# Split by topic
topic1 <- topics_long[topics_long$topic == 1,]
topic2 <- topics_long[topics_long$topic == 2,]
topic3 <- topics_long[topics_long$topic == 3,]
topic4 <- topics_long[topics_long$topic == 4,]


## Method 1: Causal Impact Model (Bayesian Structural Time Series and Counterfactual Estimation)

# Define intervention period
pre.period <- ymd_hms(c("2020-03-21 20:00:00", "2020-03-23 19:00:00")) # Periods must not overlap
post.period <- ymd_hms(c("2020-03-23 20:00:00", "2020-03-25 20:00:00"))

# Topic 1 #

# Tidy data
for_model <- zoo(cbind(topic1$n, topic1$misinformation), topic1$hour) # Left cbind in here if want to add control variables
colnames(for_model) = c("y", "x1") # Rename columns (if more than one use c("y", "x1", etc) if only one then "y")

# Run analysis
model_t1 <- CausalImpact(for_model, pre.period, post.period) # Model
plot(model_t1) # Plot results
summary(model_t1) # Get summary statistics

# Topic 2 #

# Tidy data
for_model <- zoo(cbind(topic2$n, topic2$misinformation), topic2$hour) # Left cbind in here if want to add control variables
colnames(for_model) = c("y", "x1") # Rename columns (if more than one use c("y", "x1", etc) if only one then "y")

# Run analysis
model_t2 <- CausalImpact(for_model, pre.period, post.period) # Model
plot(model_t2) # Plot results
summary(model_t2) # Get summary statistics

# Topic 3 #

# Tidy data
for_model <- zoo(cbind(topic3$n, topic3$misinformation), topic3$hour) # Left cbind in here if want to add control variables
colnames(for_model) = c("y", "x1") # Rename columns (if more than one use c("y", "x1", etc) if only one then "y")

# Run analysis
model_t3 <- CausalImpact(for_model, pre.period, post.period) # Model
plot(model_t3) # Plot results
summary(model_t3) # Get summary statistics

# Topic 4 #

# Tidy data
for_model <- zoo(cbind(topic4$n, topic4$misinformation), topic4$hour) # Left cbind in here if want to add control variables
colnames(for_model) = c("y", "x1") # Rename columns (if more than one use c("y", "x1", etc) if only one then "y")

# Run analysis
model_t4 <- CausalImpact(for_model, pre.period, post.period) # Model
plot(model_t4) # Plot results
summary(model_t4) # Get summary statistics


## Method 2: Poisson Regression Model

# Note: make sure have run source("./Scripts/negativebinomial_modelling.R") before

# Misinformation - topic 1 
eqt1 <- n ~ offset(log(misinformation)) + intervention + ns(hour)
mt1 <- glmmTMB(eqt1, 
               data = topic1,
               family=nbinom2
)
summary(mt1)
exp(confint(mt1)) # confidence intervals

# analysis of residuals
topic1$mt1_res <- residuals(mt1, type = "pearson")

# check autocorrelation
plot_acf_diagnostics(.data = topic1, 
                     .date = hour,
                     .value = mt1_res, 
                     .lags = 10,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)

# Misinformation - topic 2
eqt2 <- n ~ offset(log(misinformation)) + intervention + ns(hour)
mt2 <- glmmTMB(eqt2, 
               data = topic2,
               family=nbinom2
)
summary(mt2)
exp(confint(mt2)) # confidence intervals

# analysis of residuals
topic2$mt2_res <- residuals(mt2, type = "pearson")

# check autocorrelation
plot_acf_diagnostics(.data = topic2, 
                     .date = hour,
                     .value = mt2_res, 
                     .lags = 10,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)

# Misinformation - topic 3
eqt3 <- n ~ offset(log(misinformation)) + intervention + ns(hour)
mt3 <- glmmTMB(eqt3, 
               data = topic3,
               family=nbinom2
)
summary(mt3)
exp(confint(mt3)) # confidence intervals

# analysis of residuals
topic3$mt3_res <- residuals(mt3, type = "pearson")

# check autocorrelation
plot_acf_diagnostics(.data = topic3, 
                     .date = hour,
                     .value = mt3_res, 
                     .lags = 10,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)

# Misinformation - topic 3
eqt4 <- n ~ offset(log(misinformation)) + intervention + ns(hour)
mt4 <- glmmTMB(eqt4, 
               data = topic4,
               family=nbinom2
)
summary(mt4)
exp(confint(mt4)) # confidence intervals

# analysis of residuals
topic4$mt4_res <- residuals(mt4, type = "pearson")

# check autocorrelation
plot_acf_diagnostics(.data = topic4, 
                     .date = hour,
                     .value = mt4_res, 
                     .lags = 10,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)


## Method 3: ARIMA (time-series regression)

# Topic 1
preds <- cbind(topic1$misinformation, topic1$intervention) # Define predictor variables
colnames(preds) <- c("misinformation", "intervention")
model3_t1 <- auto.arima(topic1$n, xreg = preds) # Fit best fitting time series model
model3_t1 # Print model
coeftest(model3_t1) # Grab coefficients and model summary
confint(model3_t1) # Confidence Intervals
checkresiduals(model3_t1) # Check for auto-correlation

# Topic 2
preds <- cbind(topic2$misinformation, topic2$intervention) # Define predictor variables
colnames(preds) <- c("misinformation", "intervention")
model3_t2 <- auto.arima(topic2$n, xreg = preds) # Fit best fitting time series model
model3_t2 # Print model
coeftest(model3_t2) # Grab coefficients and model summary
confint(model3_t2) # Confidence Intervals
checkresiduals(model3_t2) # Check for auto-correlation

# Topic 3
preds <- cbind(topic3$misinformation, topic3$intervention) # Define predictor variables
colnames(preds) <- c("misinformation", "intervention")
model3_t3 <- auto.arima(topic3$n, xreg = preds) # Fit best fitting time series model
model3_t3 # Print model
coeftest(model3_t3) # Grab coefficients and model summary
confint(model3_t3) # Confidence Intervals
checkresiduals(model3_t3) # Check for auto-correlation

# Topic 4
preds <- cbind(topic4$misinformation, topic4$intervention) # Define predictor variables
colnames(preds) <- c("misinformation", "intervention")
model3_t4 <- auto.arima(topic4$n, xreg = preds) # Fit best fitting time series model
model3_t4 # Print model
coeftest(model3_t4) # Grab coefficients and model summary
confint(model3_t4) # Confidence Intervals
checkresiduals(model3_t4) # Check for auto-correlation

