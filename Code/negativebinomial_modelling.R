### Big Data & Society paper: Interrupted Time Series Analysis ###

### The code below is to fit a multilevel poisson regression model.

# Libraries
library(data.table)
library(lubridate)
library(forecast)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tsModel)
library(lmtest)
library(tidyr)
library(zoo)
library(timetk)
library(lme4)
library(merTools)
library(glmmTMB) # fitting generalised linear mixed models
library(bbmle) # general maximum likelihood estimation
library(performance) # computing indices of model quality and goodness of fit
library(splines)

# 1. Load data
tw_hour <- read.csv("./Data/tweets_per_hour.csv")
tw_hour$hour <- ymd_hms(tw_hour$hour) 
tw_hour$hour_factor <- as.factor(tw_hour$hour)
tw_hour_long <- read.csv("./Data/tweets_per_hour_long.csv")
tw_minute <- read.csv("./Data/tweets_per_min.csv")
tw_minute$minute <- ymd_hms(tw_minute$minute) 
tw_minute$hour <- floor_date(tw_minute$minute, "hour")
tw_minute_long <- read.csv("./Data/tweets_per_min_long.csv")


# 2. Identifying temporal dependence
plot_acf_diagnostics(.data = tw_hour, 
                     .date = hour,
                     .value = misinformation, 
                     .lags = 20,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)
                     
plot_acf_diagnostics(.data = tw_minute, 
                     .date = minute,
                     .value = misinformation, 
                     .lags = 20,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)

plot_acf_diagnostics(.data = tw_minute, 
                     .date = minute,
                     .value = bots, 
                     .lags = 20,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)

plot_acf_diagnostics(.data = tw_minute, 
                     .date = minute,
                     .value = bots, 
                     .lags = 20,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)


# 3. Negative Binomial modelling

# 3.1 Misinformation - hours
  #estimate nb model
eq1 <- misinformation ~ offset(log(tweets)) + intervention + ns(hour)
m1 <- glmmTMB(eq1, 
              data = tw_hour,
              family=nbinom2
)
summary(m1)
exp(confint(m1)) # confidence intervals

  # analysis of residuals
tw_hour$m1_res <- residuals(m1, type = "pearson")

  # check autocorrelation
plot_acf_diagnostics(.data = tw_hour, 
                     .date = hour,
                     .value = m1_res, 
                     .lags = 20,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)

#############
# 3.2 Misinformation - minutes
#estimate nb model
eq2 <- misinformation ~ offset(log(tweets)) + intervention + ns(minute)
m2 <- glmmTMB(eq2, 
              data = tw_minute,
              family=nbinom2
)
summary(m2)
exp(confint(m2)) # confidence intervals

# analysis of residuals
tw_minute$m2_res <- residuals(m2, type = "pearson")

# check autocorrelation
plot_acf_diagnostics(.data = tw_minute, 
                     .date = minute,
                     .value = m2_res, 
                     .lags = 10,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)

#############
# 3.3 Bots - hours
#estimate nb model
eq3 <- bots ~ offset(log(tweets)) + intervention + ns(hour)
m3 <- glmmTMB(eq3, 
              data = tw_hour,
              family=nbinom2
)
summary(m3)
exp(confint(m3)) # confidence intervals

# analysis of residuals
tw_hour$m3_res <- residuals(m3, type = "pearson")

# check autocorrelation
plot_acf_diagnostics(.data = tw_hour, 
                     .date = hour,
                     .value = m3_res, 
                     .lags = 10,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)

#############
# 3.4 Bots - minutes
#estimate nb model
eq4 <- bots ~ offset(log(tweets)) + intervention + ns(minute)
m4 <- glmmTMB(eq4, 
              data = tw_minute,
              family=nbinom2
)
summary(m4)
exp(confint(m4)) # confidence intervals

# analysis of residuals
tw_minute$m4_res <- residuals(m4, type = "pearson")

# check autocorrelation
plot_acf_diagnostics(.data = tw_minute, 
                     .date = minute,
                     .value = m4_res, 
                     .lags = 10,
                     .line_size = 2,
                     .show_white_noise_bars = TRUE,
                     .interactive = FALSE)


### 4. Plotting counterfactual
tw_hour_train <- tw_hour %>% filter(intervention==0)
tw_hour_test <- tw_hour %>% filter(intervention==1)
eq1c <- misinformation ~ offset(log(tweets)) + ns(hour)
m1c <- glmmTMB(eq1c, 
              data = tw_hour_train,
              family=nbinom2
)
summary(m1c)
exp(confint(m1c)) # confidence intervals

  # Extract Predictions
tw_hour_train$prediction <- tw_hour_train$misinformation
tw_hour_train$prediction.cimin <- NA
tw_hour_train$prediction.cimax <- NA
#tw_hour_test$p_misinformation <- round(exp(predict(m1c, tw_hour_test, se.fit = TRUE)))
temp <- predict(m1c, tw_hour_test, se.fit = TRUE)
tw_hour_test$prediction <- round(exp(temp$fit))
tw_hour_test$prediction.cimin = round(exp(temp$fit-1.96*temp$se.fit))
tw_hour_test$prediction.cimax = round(exp(temp$fit+1.96*temp$se.fit))

  # new data frame
tw_hour_pr <- rbind(tw_hour_train, tw_hour_test)

  # plot
p1 <-  ggplot(tw_hour_pr, aes(x = hour, y = prediction)) + 
  geom_line(aes(x = hour, y = prediction), size=1.5, color="grey50") +
  geom_line(aes(x = hour, y = misinformation), size=1.5, color="black") +
  geom_ribbon(aes(ymin = prediction.cimin, ymax =prediction.cimax), linetype = 2, alpha=0.2) +
  theme(legend.position = "none") +
  labs(x= "Hour",
       y = "Misinformation (Number of tweets)")

