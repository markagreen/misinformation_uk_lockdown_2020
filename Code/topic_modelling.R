#########################
#### Topic modelling ####
#########################

# Purpose: Classify tweets identified as misinformation to describe 'types of misinformation tweets'.

# Libraries
# Data manipulation
library(dplyr)
library(tm)
library(tidytext)
library(tidyr)
library(lubridate)
library(data.table)
#library(magrittr)
# Visualisation
library(ggplot2)
library(ggrepel)
# Analysis
library(topicmodels)
library(ldatuning)


### Data wrangling ###


# Load data
#all_tweets <- read.csv("./Data/lockdown_misinformation_merge.csv", header = TRUE) 
all_tweets <- read.csv("/Users/markagreen/Documents/Data/Twitter/lockdown_misinformation_merge.csv", header = TRUE)
misinformation <- all_tweets[all_tweets$misinformation == 1,] # Subset only tweets identified as misinformation
rm(all_tweets)

# Hold date for later
timestamp <- misinformation[,c("id", "created_at")]
timestamp$created_at <- as.POSIXct(timestamp$created_at, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")  # Convert to time-date format

# Convert data to a corpus
hold <- misinformation[,c("id", "full_text_cleaned")] # Subset required information
# Corpus requires following column names
names(hold)[names(hold) == "id"] <- "doc_id"
names(hold)[names(hold) == "full_text_cleaned"] <- "text"
corpus <- Corpus(DataframeSource(hold)) # Convert
rm(hold)

# Tidy up corpus
corpus <- corpus %>%
  tm_map(removePunctuation) %>% # Remove punctuation
  #tm_map(removeNumbers) %>% # Remove numbers (not done this here as want to identify 5G)
  tm_map(removeWords, stopwords('en')) # %>% # Remove stope words that have little meaning e.g. and, the, of etc

# Convert corpus to document term matrix
dtm <- DocumentTermMatrix(corpus)
rm(corpus)


### Analysis ###


## Classfying tweets using Latent Dirichlet Allocation ##

# Tidy up the DTM through getting rid of documents (tweets) with zero terms in it
ui <- unique(dtm$i) # Identify unique documents
dtm_new <- dtm[ui,] # Subset

# Calculate model fit for varying number of groups (slow)
result <- FindTopicsNumber(
  dtm_new,
  topics = seq(from = 2, to = 15, by = 1), # Select range of topics to classify
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"), # Select metrics
  method = "Gibbs", # Gibbs sampling
  control = list(seed = 281190), # Set seed so can replicate
  verbose = TRUE
)

# Plot metrics
FindTopicsNumber_plot(result)
ggsave("./Plots/topic_models_fit.tiff", dpi=300)
write.csv(result, "./Data/topic_models_fit.csv") # Save metrics

# Classify tweets
lda_results <- LDA(dtm_new, k = 4, control = list(seed = 281190), method = "Gibbs")


## Understand what topics are ##

# Plot topics to describe/interpret them
lda_topics <- tidy(lda_results, matrix = "beta") # Tidy results

# Identify terms that best describe tweets
lda_top_terms <- lda_topics %>% 
  group_by(topic) %>% # By topic (or cluster)
  top_n(25, beta) %>% # Select ten most important terms
  ungroup() %>%
  arrange(topic, -beta)

# Plot the results of above
lda_top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
ggsave("./Plots/topic_term_prob.tiff", dpi=300)

# # Calculate the terms with the greatest difference between groups
# # Tidy data
# beta_spread <- lda_topics %>%
#   mutate(topic = paste0("topic", topic)) %>%
#   spread(topic, beta)
# 
# # Plot
# beta_spread %>% 
#   ggplot(aes(topic1, topic2)) + # Scatter plot of terms across topics 1 and 2
#   geom_point(alpha = 0.1) +
#   geom_text_repel(data=subset(beta_spread, topic1 < 0.0025 & topic2 > 0.0025),
#             aes(topic1, topic2,label=term),colour="red") + # Plot labels for most different topic 2 terms
#   geom_text_repel(data=subset(beta_spread, topic1 > 0.0025 & topic2 < 0.0025),
#                   aes(topic1, topic2,label=term),colour="blue") + # Plot labels for most different topic 1 terms
#   coord_cartesian(xlim = c(0,0.01), ylim = c(0,0.01)) + # Don't display outliers
#   xlab("Topic 1: Term Frequency") +
#   ylab("Topic 2: Term Frequency")
# 
# # Then repeat for all combinations of topics...


## Plot trends in topics ##

# Select probability of each tweet belonging to each topic (unfinished)
lda_docs <- tidy(lda_results, matrix = "gamma")

# Aggregate to take highest probabilty for topic
lda_agg <- lda_docs %>% 
  group_by(document) %>% 
  filter(gamma == max(gamma))

# Join back on timestamp
timestamp$id <- as.character(timestamp$id) # To match variable type
lda_agg <- merge(lda_agg, timestamp, by.x = "document", by.y = "id", all.x = TRUE)

# Plot trend
lda_agg$hour <- cut(lda_agg$created_at, breaks="hour") # Split by hour
lda_agg$min <- cut(lda_agg$created_at, breaks="min") # Split by minute
hour_avg <- lda_agg %>% group_by(hour) %>% count(topic) # Count topics by hour
min_avg <- lda_agg %>% group_by(min) %>% count(topic) # Count topics by mi
hour_avg$hour <- ymd_hms(hour_avg$hour) # Convert to date format
min_avg$min <- ymd_hms(hour_avg$min) 

# Plot
# Smoothed (slightly misleading)
ggplot(hour_avg, aes(x = hour, y = n, color=factor(topic))) +
  geom_path(alpha=0.5) +
  geom_smooth(method = "gam", se = F) +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  ylim(0,200) +
  ylab("Number of tweets") +
  xlab("Date") +
  labs(color = "Topic")
ggsave("./Plots/topic_trend_smoothed.tiff", dpi=300)

# Not smoothed
ggplot(hour_avg, aes(x = hour, y = n, color=factor(topic))) +
  geom_path() +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  ylim(0,200) +
  ylab("Number of tweets") +
  xlab("Date") +
  labs(color = "Topic")
ggsave("./Plots/topic_trend.tiff", dpi=300)

# Faceted
ggplot(hour_avg, aes(x = hour, y = n)) +
  geom_line() + # Consider just geom_path()
  facet_grid(vars(topic)) + # Add scales = "free" if want to have different axies
  #geom_smooth(method = "gam", se = F) +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  #ylim(0,200) +
  ylab("Number of tweets") +
  xlab("Date")
ggsave("./Plots/topic_trend_facet.tiff", dpi=300)

# As percentage of all tweets in the hour
hour_tot <- aggregate(hour_avg$n~hour, hour_avg, sum)
hour_avg <- merge(hour_avg, hour_tot, by = "hour")
hour_avg$pc_per_hour <- (hour_avg$n / hour_avg$`hour_avg$n`)  * 100

# Smoothed
hour_avg %>%
  ggplot(aes(hour, pc_per_hour, group=factor(topic), colour=factor(topic))) +
  geom_path(alpha = 0.5) +
  geom_smooth(aes(group=factor(topic)), method = "gam", se=F) +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  ylim(0,100) +
  ylab("Percentage of all hourly tweets") +
  xlab("Date") +
  labs(color = "Topic")
ggsave("./Plots/topic_trend_pc_smooth.tiff", dpi=300)

# Not smoothed
hour_avg %>%
  ggplot(aes(hour, pc_per_hour, group=factor(topic), colour=factor(topic))) +
  geom_path() +
  geom_vline(xintercept = as.numeric(ymd_hms("2020-03-23 20:00:00")), linetype="dotted") +
  ylim(0,100) +
  ylab("Percentage of all hourly tweets") +
  xlab("Date") +
  labs(color = "Topic")
ggsave("./Plots/topic_trend_pc.tiff", dpi=300)


## Check role of bots ##
misinformation$id <- as.character(misinformation$id) # To match variable type
misinformation <- merge(misinformation, lda_agg, timestamp, by.x = "id", by.y = "document", all.x = TRUE) # Merge datasets
tbl <- table(misinformation$bot, misinformation$topic) # Frequency table
tbl # Print
chisq.test(tbl) # Chi squared test


## Check retweets and sharing of topics
# Convert to numeric as factors
misinformation$retweet_count <- as.numeric(as.character(misinformation$retweet_count))
misinformation$reply_count <- as.numeric(as.character(misinformation$reply_count))
misinformation$quote_count <- as.numeric(as.character(misinformation$quote_count))
misinformation$favorite_count <- as.numeric(as.character(misinformation$favorite_count))
misinformation$engagement <- misinformation$quote_count + misinformation$reply_count + misinformation$retweet_count + misinformation$favorite_count

# Mean share by topic
dtl <- data.table(misinformation)
dtl[, list(engagement = mean(engagement, na.rm = TRUE)), by = "topic"] # Calculate mean count by topic
summary(aov(engagement ~ topic, data = misinformation)) # one way anova test


## Save model ##
model <- misinformation[,c("id", "topic")]
write.csv(model, "./Data/lda_topic_id.csv")
