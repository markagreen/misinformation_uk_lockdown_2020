#########################
##### Identify bots #####
#########################

# Purpose: To classify Twitter users as bots or not.

# Libraries
# devtools::install_github("mkearney/botrnot") # This didn't work for me so needed to follow https://github.com/mkearney/tweetbotornot/issues/24
# remove.packages("tweetbotornot")
# install_github("markagreen/tweetbotornot", dependencies = TRUE) # Anyone should be able to install this
library(tweetbotornot)
#devtools::install_github("mkearney/rtweet")
library(rtweet)
library(dplyr)
library(httr)

# Set up Twitter details
consumer_key <- "VzihIPxv5oFrd3SkNuBuQk9o3"
consumer_secret <- "Iq7hi4K1cZnzgD3RC1miTM6rcrHMA4aeHj3OeCsI9OvFVtX5Ej"
access_token <- "2507558052-K6abCbi1LD59qPxIMtlsGVWL7dDdLkfyQYPuLFS"
access_secret <- "x9AwwJuLJWWdpSZMPkKGG4RAtxtZh9uXxWfebj5HsseWA"
app <- "Liv_misinformation_study"

token <- create_token(
  app = app,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret,
  set_renv = TRUE)

rm(consumer_key, consumer_secret, access_secret, access_token, app)

# Create list of usernames in project

# January
tweets <- read.csv("../Data/twitter_covid_jan_small_cleaned.csv", header = TRUE) # Load data
users <- as.data.frame(tweets$username) # Subset usernames
users <- distinct(users) # Remove duplicates (i.e. multiple tweets within a month)
rm(tweets) # Delete

# February
tweets <- read.csv("../Data/twitter_covid_feb_small_cleaned.csv", header = TRUE) # Load data
temp <- as.data.frame(tweets$username) # Subset usernames
temp <- distinct(temp) # Remove duplicates
users <- rbind(users, temp) # Join on to longer list
users <- distinct(users) # Remove duplicates again
rm(tweets, temp)

# March
tweets <- read.csv("../Data/twitter_covid_mar_small_cleaned.csv", header = TRUE) # Load data
temp <- as.data.frame(tweets$username) # Subset usernames
temp <- distinct(temp) # Remove duplicates
users <- rbind(users, temp) # Join on to longer list
users <- distinct(users) # Remove duplicates again
rm(tweets, temp) 

# Save
write.csv(users, "./users.csv")

# Estimate likelihood of being a bot

# Running code after running above
# user_list <- as.character(users$`tweets$username`)
# predict_bots <- tweetbotornot(user_list, fast = TRUE)

# Loading in csv file
users <- read.csv("./users.csv")
user_list <- as.character(users$x)
# Split data into 90k subsets as can only run that at a time (Twitter limit)
user_list1 <- user_list[1:90000]
user_list2 <- user_list[90001:180000]
user_list3 <- user_list[180001:270000]
user_list4 <- user_list[270001:360000]
user_list5 <- user_list[360001:450000]
user_list6 <- user_list[450001:508034]

# Predict bot likelihood
predict_bots1 <- tweetbotornot(user_list1, fast = TRUE)
predict_bots2 <- tweetbotornot(user_list2, fast = TRUE)
predict_bots3 <- tweetbotornot(user_list3, fast = TRUE)
predict_bots4 <- tweetbotornot(user_list4, fast = TRUE)
predict_bots5 <- tweetbotornot(user_list5, fast = TRUE)
predict_bots6 <- tweetbotornot(user_list6, fast = TRUE)

# Join back together
predict_bots <- rbind(predict_bots1, predict_bots2, predict_bots3, predict_bots4, predict_bots5, predict_bots6)
write.csv(predict_bots, "./predict_bots.csv") # Save
rm(list = ls()) # Remove all files
gc()

# Note:
# The default [gradient boosted] model uses both users-level (bio, location, number of followers and friends, etc.) and tweets-level (number of hashtags, mentions, capital letters, etc. in a user’s most recent 100 tweets) data to estimate the probability that users are bots. For larger data sets, this method can be quite slow. Due to Twitter’s REST API rate limits, users are limited to only 180 estimates per every 15 minutes.
# To maximize the number of estimates per 15 minutes (at the cost of being less accurate), use the fast = TRUE argument. This method uses only users-level data, which increases the maximum number of estimates per 15 minutes to 90,000! Due to losses in accuracy, this method should be used with caution!
# I will use the fast method first (180 users per 15 mins means it would take ~30 days), and then update with the slower method when can run it longer

# Test code
users <- c("realdonaldtrump", "netflix_bot",
           "kearneymw", "dataandme", "hadleywickham",
           "ma_salmon", "juliasilge", "tidyversetweets", 
           "American__Voter", "mothgenerator", "hrbrmstr")

## get botornot estimates
bot_list <- tweetbotornot(users, fast = FALSE)
