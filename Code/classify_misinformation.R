#### Classifying tweets as misinformation ###

# Libraries

# Load data
jan <- read.csv("./Data/twitter_covid_jan_small_cleaned.csv", header = TRUE) # January
feb <- read.csv("./Data/twitter_covid_feb_small_cleaned.csv", header = TRUE) # February
mar <- read.csv("./Data/twitter_covid_mar_small_cleaned.csv", header = TRUE) # March
lockdown <- read.csv("./Data/twitter_covid_lockdown_mar2020_small_cleaned.csv", header = TRUE) # March

# Tidy
lockdown$X.3 <- NA
lockdown$X.2 <- NA
lockdown$X.1 <- NA

# Join together
all_months <- rbind(jan, feb, mar, lockdown)
#backup <- all_months # As takes ages to load!
rm(jan, feb, mar, lockdown)

## Part 1: Not true but not false misinformation ##

# Misinformation lookup
lkup <- read.csv("./Data/not_true_not_false.csv", header = TRUE)
lkup$twitter_url <-gsub("\\?amp=1.*","",lkup$twitter_url) # Remove part of url not needed
# Save short links as lookup
lkup_urls_short <- lkup[,c("twitter_url", "not_true_not_false")] # Keep required vars
lkup_urls_short <- lkup_urls_short[lkup_urls_short$twitter_url != "",] # Drop missing rows
# Save full links as lookup
lkup_urls_full <- lkup[,c("original_url", "not_true_not_false")] # Keep required vars
lkup_urls_full <- lkup_urls_full[lkup_urls_full$original_url != "",] # Drop missing rows
# Save tweets as seperate lookup
lkup_tw <- lkup[,c("tweet_id", "not_true_not_false")] # Keep required vars
lkup_tw <- lkup_tw[!is.na(lkup_tw$tweet_id),] # Drop missing rows
rm(lkup)

# Classify tweets

# By Tweets

# User tweets
all_months <- merge(all_months, lkup_tw, by.x = "id", by.y = "tweet_id", all.x = TRUE) # Join on misinformation tweets lookup to main data
all_months$not_true_not_false[is.na(all_months$not_true_not_false)] <- 0 # Recode variable as 0 if not a match
all_months$not_true_not_false_tweet1 <- all_months$not_true_not_false # Rename variable
all_months$not_true_not_false <- NULL # Drop variable as no longer needed

# Retweeted tweets
all_months$rt_tweet_id <- as.numeric(all_months$rt_tweet_id) # Recode as numeric as stored as 'list'
all_months <- merge(all_months, lkup_tw, by.x = "rt_tweet_id", by.y = "tweet_id", all.x = TRUE)
all_months$not_true_not_false[is.na(all_months$not_true_not_false)] <- 0
all_months$not_true_not_false_tweet2 <- all_months$not_true_not_false
all_months$not_true_not_false <- NULL

# Quoted tweets
all_months$qt_tweet_id <- as.numeric(all_months$qt_tweet_id)
all_months <- merge(all_months, lkup_tw, by.x = "qt_tweet_id", by.y = "tweet_id", all.x = TRUE)
all_months$not_true_not_false[is.na(all_months$not_true_not_false)] <- 0
all_months$not_true_not_false_tweet3 <- all_months$not_true_not_false
all_months$not_true_not_false <- NULL

# Combine together into single variable
all_months$not_true_not_false_tweet <- 0
all_months$not_true_not_false_tweet[all_months$not_true_not_false_tweet1 == 1 | 
                                      all_months$not_true_not_false_tweet2 == 1 | 
                                      all_months$not_true_not_false_tweet3 == 1] <- 1
all_months$not_true_not_false_tweet1 <- NULL
all_months$not_true_not_false_tweet2 <- NULL
all_months$not_true_not_false_tweet3 <- NULL

table(all_months$not_true_not_false_tweet) # n=1778

# By URL Links

# Links extracted from tweets
all_months$url1 <- as.character(all_months$url1) # Change to same type (char)
lkup_urls_short$twitter_url <- as.character(lkup_urls_short$twitter_url)
all_months <- merge(all_months, lkup_urls_short, by.x = "url1", by.y = "twitter_url", all.x = TRUE) # Join on lookup
all_months$not_true_not_false[is.na(all_months$not_true_not_false)] <- 0 # Recode variable as 0 if not a match
all_months$not_true_not_false_url1 <- all_months$not_true_not_false # Rename variable
all_months$not_true_not_false <- NULL # Drop variable as no longer needed

all_months$url2 <- as.character(all_months$url2)
all_months <- merge(all_months, lkup_urls_short, by.x = "url2", by.y = "twitter_url", all.x = TRUE) # Repeat for other URLs
all_months$not_true_not_false[is.na(all_months$not_true_not_false)] <- 0
all_months$not_true_not_false_url2 <- all_months$not_true_not_false
all_months$not_true_not_false <- NULL

all_months$url3 <- as.character(all_months$url3)
all_months <- merge(all_months, lkup_urls_short, by.x = "url3", by.y = "twitter_url", all.x = TRUE)
all_months$not_true_not_false[is.na(all_months$not_true_not_false)] <- 0
all_months$not_true_not_false_url3 <- all_months$not_true_not_false
all_months$not_true_not_false <- NULL

# URLs extracted from Twitter's raw tweet information

# Retweeted short URL
lkup_urls_full$original_url <- as.character(lkup_urls_full$original_url)
all_months$rt_url <- as.character(all_months$rt_url)
all_months <- merge(all_months, lkup_urls_short, by.x = "rt_url", by.y = "twitter_url", all.x = TRUE)
all_months$not_true_not_false[is.na(all_months$not_true_not_false)] <- 0
all_months$not_true_not_false_rturl <- all_months$not_true_not_false
all_months$not_true_not_false <- NULL

# Retweeted full URL
all_months$rt_expanded_url <- as.character(all_months$rt_expanded_url)
all_months <- merge(all_months, lkup_urls_full, by.x = "rt_expanded_url", by.y = "original_url", all.x = TRUE)
all_months$not_true_not_false[is.na(all_months$not_true_not_false)] <- 0
all_months$not_true_not_false_rturlfull <- all_months$not_true_not_false
all_months$not_true_not_false <- NULL

# Quoted short URL
all_months$qt_url <- as.character(all_months$qt_url)
all_months <- merge(all_months, lkup_urls_short, by.x = "qt_url", by.y = "twitter_url", all.x = TRUE)
all_months$not_true_not_false[is.na(all_months$not_true_not_false)] <- 0
all_months$not_true_not_false_qturl <- all_months$not_true_not_false
all_months$not_true_not_false <- NULL

# Quoted full URL
all_months$qt_expanded_url <- as.character(all_months$qt_expanded_url)
all_months <- merge(all_months, lkup_urls_full, by.x = "qt_expanded_url", by.y = "original_url", all.x = TRUE)
all_months$not_true_not_false[is.na(all_months$not_true_not_false)] <- 0
all_months$not_true_not_false_qturlfull <- all_months$not_true_not_false
all_months$not_true_not_false <- NULL

# Combine into single variable
all_months$not_true_not_false_url <- 0
all_months$not_true_not_false_url[all_months$not_true_not_false_url1 == 1 | 
                                    all_months$not_true_not_false_url2 == 1 | 
                                    all_months$not_true_not_false_url3 == 1 | 
                                    all_months$not_true_not_false_rturl == 1 | 
                                    all_months$not_true_not_false_rturlfull == 1 | 
                                    all_months$not_true_not_false_qturl == 1 | 
                                    all_months$not_true_not_false_qturlfull == 1] <- 1
table(all_months$not_true_not_false_url) # n=90
all_months[35:41] <- NULL # Tidy up

# Extract not true not false tweets
ntnf_tweets <- all_months[all_months$not_true_not_false_url == 1 | all_months$not_true_not_false_tweet == 1,]
ntnf_tweets$rt_username <- as.character(ntnf_tweets$rt_username) # Else will not save as csv
ntnf_tweets$qt_username <- as.character(ntnf_tweets$qt_username)
write.csv(ntnf_tweets, "./Data/ntnf_tweets.csv")


## Part 2: False misinformation ##

# Misinformation lookup
lkup <- read.csv("./Data/false_info.csv", header = TRUE)
lkup$false_urls <- lkup$false_info
lkup$twitter_url <-gsub("\\?amp=1.*","",lkup$twitter_url) # Remove part of url not needed
# Save short links as lookup
lkup_urls_short <- lkup[,c("twitter_url", "false_urls")] # Keep required vars
lkup_urls_short <- lkup_urls_short[lkup_urls_short$twitter_url != "",] # Drop missing rows
# Save full links as lookup
lkup_urls_full <- lkup[,c("original_url", "false_urls")] # Keep required vars
lkup_urls_full <- lkup_urls_full[lkup_urls_full$original_url != "",] # Drop missing rows
# Save tweets as seperate lookup
lkup_tw <- lkup[,c("tweet_id", "false_urls")] # Keep required vars
lkup_tw <- lkup_tw[!is.na(lkup_tw$tweet_id),] # Drop missing rows
rm(lkup)

# Classify tweets

# By Tweets

# User tweets
all_months <- merge(all_months, lkup_tw, by.x = "id", by.y = "tweet_id", all.x = TRUE) # Join on misinformation tweets lookup to main data
all_months$false_urls[is.na(all_months$false_urls)] <- 0 # Recode variable as 0 if not a match
all_months$false_urls_tweet1 <- all_months$false_urls # Rename variable
all_months$false_urls <- NULL # Drop variable as no longer needed

# Retweeted tweets
all_months$rt_tweet_id <- as.numeric(all_months$rt_tweet_id) # Recode as numeric as stored as 'list'
all_months <- merge(all_months, lkup_tw, by.x = "rt_tweet_id", by.y = "tweet_id", all.x = TRUE)
all_months$false_urls[is.na(all_months$false_urls)] <- 0
all_months$false_urls_tweet2 <- all_months$false_urls
all_months$false_urls <- NULL

# Quoted tweets
all_months$qt_tweet_id <- as.numeric(all_months$qt_tweet_id)
all_months <- merge(all_months, lkup_tw, by.x = "qt_tweet_id", by.y = "tweet_id", all.x = TRUE)
all_months$false_urls[is.na(all_months$false_urls)] <- 0
all_months$false_urls_tweet3 <- all_months$false_urls
all_months$false_urls <- NULL

# Combine together into single variable
all_months$false_urls_tweet <- 0
all_months$false_urls_tweet[all_months$false_urls_tweet1 == 1 | 
                                      all_months$false_urls_tweet2 == 1 | 
                                      all_months$false_urls_tweet3 == 1] <- 1
all_months$false_urls_tweet1 <- NULL
all_months$false_urls_tweet2 <- NULL
all_months$false_urls_tweet3 <- NULL

table(all_months$false_urls_tweet) # 743

# By URL Links

# Links extracted from tweets
all_months$url1 <- as.character(all_months$url1) # Change to same type (char)
lkup_urls_short$twitter_url <- as.character(lkup_urls_short$twitter_url)
all_months <- merge(all_months, lkup_urls_short, by.x = "url1", by.y = "twitter_url", all.x = TRUE) # Join on lookup
all_months$false_urls[is.na(all_months$false_urls)] <- 0 # Recode variable as 0 if not a match
all_months$false_urls_url1 <- all_months$false_urls # Rename variable
all_months$false_urls <- NULL # Drop variable as no longer needed

all_months$url2 <- as.character(all_months$url2)
all_months <- merge(all_months, lkup_urls_short, by.x = "url2", by.y = "twitter_url", all.x = TRUE) # Repeat for other URLs
all_months$false_urls[is.na(all_months$false_urls)] <- 0
all_months$false_urls_url2 <- all_months$false_urls
all_months$false_urls <- NULL

all_months$url3 <- as.character(all_months$url3)
all_months <- merge(all_months, lkup_urls_short, by.x = "url3", by.y = "twitter_url", all.x = TRUE)
all_months$false_urls[is.na(all_months$false_urls)] <- 0
all_months$false_urls_url3 <- all_months$false_urls
all_months$false_urls <- NULL

# URLs extracted from Twitter's raw tweet information

# Retweeted short URL
lkup_urls_full$original_url <- as.character(lkup_urls_full$original_url)
all_months$rt_url <- as.character(all_months$rt_url)
all_months <- merge(all_months, lkup_urls_short, by.x = "rt_url", by.y = "twitter_url", all.x = TRUE)
all_months$false_urls[is.na(all_months$false_urls)] <- 0
all_months$false_urls_rturl <- all_months$false_urls
all_months$false_urls <- NULL

# Retweeted full URL
all_months$rt_expanded_url <- as.character(all_months$rt_expanded_url)
all_months <- merge(all_months, lkup_urls_full, by.x = "rt_expanded_url", by.y = "original_url", all.x = TRUE)
all_months$false_urls[is.na(all_months$false_urls)] <- 0
all_months$false_urls_rturlfull <- all_months$false_urls
all_months$false_urls <- NULL

# Quoted short URL
all_months$qt_url <- as.character(all_months$qt_url)
all_months <- merge(all_months, lkup_urls_short, by.x = "qt_url", by.y = "twitter_url", all.x = TRUE)
all_months$false_urls[is.na(all_months$false_urls)] <- 0
all_months$false_urls_qturl <- all_months$false_urls
all_months$false_urls <- NULL

# Quoted full URL
all_months$qt_expanded_url <- as.character(all_months$qt_expanded_url)
all_months <- merge(all_months, lkup_urls_full, by.x = "qt_expanded_url", by.y = "original_url", all.x = TRUE)
all_months$false_urls[is.na(all_months$false_urls)] <- 0
all_months$false_urls_qturlfull <- all_months$false_urls
all_months$false_urls <- NULL

# Combine into single variable
all_months$false_url <- 0
all_months$false_url[all_months$false_urls_url1 == 1 | 
                                    all_months$false_urls_url2 == 1 | 
                                    all_months$false_urls_url3 == 1 | 
                                    all_months$false_urls_rturl == 1 | 
                                    all_months$false_urls_rturlfull == 1 | 
                                    all_months$false_urls_qturl == 1 | 
                                    all_months$false_urls_qturlfull == 1] <- 1
table(all_months$false_url) # n=208
all_months[37:43] <- NULL # Tidy up

# Extract not true not false_urls tweets
ntnf_tweets <- all_months[all_months$false_url == 1 | all_months$false_urls_tweet == 1,]
ntnf_tweets$rt_username <- as.character(ntnf_tweets$rt_username) # Else will not save as csv
ntnf_tweets$qt_username <- as.character(ntnf_tweets$qt_username)
write.csv(ntnf_tweets, "./Data/false_tweets.csv")


## Part 3: Classifying URL website source ##

# We can only do this via the expanded URLs for RTs and QTs unfortunately as main tweets only have t.co links saved

# Load list of websites associated with fake news
lkup_web <- read.csv("./Data/websites.csv") 
lkup_web$website <- tolower(lkup_web$website) # Convert to lower case 
lkup_web$fake_website <- 1 # Add variable on

# Retweets
all_months <- merge(all_months, lkup_web, by.x = "rt_website", by.y = "website", all.x = TRUE) # Join on misinformation websites
all_months$fake_website[is.na(all_months$fake_website)] <- 0 # Recode variable as 0 if not a match
all_months$fake_website_rt <- all_months$fake_website # Rename variable
all_months$fake_website <- NULL # Drop variable as no longer needed

# Quoted retweets
all_months <- merge(all_months, lkup_web, by.x = "qt_website", by.y = "website", all.x = TRUE) # Join on misinformation websites
all_months$fake_website[is.na(all_months$fake_website)] <- 0 # Recode variable as 0 if not a match
all_months$fake_website_qt <- all_months$fake_website # Rename variable
all_months$fake_website <- NULL # Drop variable as no longer needed

table(all_months$fake_website_rt) # n=4035
table(all_months$fake_website_qt) # n=210

# Extract tweets
fweb_tweets <- all_months[all_months$fake_website_rt == 1 | all_months$fake_website_qt == 1,]
fweb_tweets$rt_username <- as.character(fweb_tweets$rt_username) # Else will not save as csv
fweb_tweets$qt_username <- as.character(fweb_tweets$qt_username)
write.csv(fweb_tweets, "./Data/fake_web_tweets.csv")


## Part 4: Identifying accounts associated with active spread of misinformation ##

# Load lookup
lkup_acc <- read.csv("./Data/misinformation_users.csv", header = TRUE)
lkup_acc$misinfo_user <- 1 # For lookup later

# User tweets
all_months <- merge(all_months, lkup_acc, by.x = "username", by.y = "username", all.x = TRUE) # Join on misinformation tweets lookup to main data
all_months$misinfo_user[is.na(all_months$misinfo_user)] <- 0 # Recode variable as 0 if not a match
all_months$misinfo_user_tweet <- all_months$misinfo_user # Rename variable
all_months$misinfo_user <- NULL # Drop variable as no longer needed

# Retweeted tweets
#all_months$rt_username <- as.character(all_months$rt_username)
all_months <- merge(all_months, lkup_acc, by.x = "rt_username", by.y = "username", all.x = TRUE)
all_months$misinfo_user[is.na(all_months$misinfo_user)] <- 0
all_months$misinfo_user_rt <- all_months$misinfo_user
all_months$misinfo_user <- NULL

# Quoted tweets
all_months <- merge(all_months, lkup_acc, by.x = "qt_username", by.y = "username", all.x = TRUE)
all_months$misinfo_user[is.na(all_months$misinfo_user)] <- 0
all_months$misinfo_user_qt <- all_months$misinfo_user
all_months$misinfo_user <- NULL

# Combine together into single variable
all_months$misinfo_user <- 0
all_months$misinfo_user[all_months$misinfo_user_tweet == 1 | 
                              all_months$misinfo_user_rt == 1 | 
                              all_months$misinfo_user_qt == 1] <- 1
all_months$misinfo_user_tweet <- NULL
all_months$misinfo_user_rt <- NULL
all_months$misinfo_user_qt <- NULL

table(all_months$misinfo_user) # n=4905

# Extract tweets
misinfo_user_tweets <- all_months[all_months$misinfo_user == 1,]
misinfo_user_tweets$rt_username <- as.character(misinfo_user_tweets$rt_username) # Else will not save as csv
misinfo_user_tweets$qt_username <- as.character(misinfo_user_tweets$qt_username)
write.csv(misinfo_user_tweets, "./Data/misinfo_user_tweets.csv")


## Part 5: Identifying tweets matching WHO keywords associated with misinformation ##

# Convert data to a corpus
hold <- all_months[,c("id", "full_text_cleaned")] # Subset required information
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
#corpus <- tm_map(corpus, PlainTextDocument) 
#corpus <- Corpus(VectorSource(corpus))
#tm_map(stripWhitespace) # Remove whitespace

# Convert corpus to document term matrix
dtm <- DocumentTermMatrix(corpus)
rm(corpus)

# Tidy data
dtm_td <- tidy(dtm)
rm(dtm)

# Load lookup and add onto list of terms
lkup_who <- read.csv("./Data/who_keywords.csv")
dtm_td <- merge(dtm_td, lkup_who, by.x = "term", by.y = "keyword", all.x = TRUE)

# Create variables for whether match each of the types of misinformation
dtm_td$cause <- 0
dtm_td$cause[dtm_td$form == "cause"] <- 1
dtm_td$transmission <- 0
dtm_td$transmission[dtm_td$form == "transmission"] <- 1
dtm_td$treatment <- 0
dtm_td$treatment[dtm_td$form == "treatment"] <- 1

# Aggregate up to tweet id so can be rejoined back onto main dataset
dtm_td <- data.table(dtm_td)
lkup_who_tweets <- dtm_td[, list(cause = sum(cause), transmission = sum(transmission), treatment = sum(treatment)), by = document]
names(lkup_who_tweets)[names(lkup_who_tweets) == "document"] <- "id"
write.csv(lkup_who_tweets, "./Data/lookup_who_terms_tweets.csv")


## Part 6: Iterating through retweets of retweets ##

# Taking all of the tweets that we identified as talking about misinformation, we pull out subsequent retweets of them, and then the retweets of those, and so on... To give us the cascading discussion of tweets. Snowballing data collection.

# Load all misinformation lookups and and subset tweet ids
lkup1 <- read.csv("./Data/ntnf_tweets.csv") # Load
lkup1$misinformation <- 1 # Add in required variables for consistent lookup
lkup1$not_true_not_false <- 1 
lkup1$false <- NA
lkup1$cause <- NA
lkup1$transmission <- NA
lkup1$treatment <- NA
lkup1 <- lkup1[,c("id", "misinformation", "not_true_not_false", "false", "cause", "transmission", "treatment")] # Subset required variables

lkup2 <- read.csv("./Data/false_tweets.csv") # Repeat process
lkup2$misinformation <- 1 
lkup2$not_true_not_false <- NA
lkup2$false <- 1
lkup2$cause <- NA
lkup2$transmission <- NA
lkup2$treatment <- NA
lkup2 <- lkup2[,c("id", "misinformation", "not_true_not_false", "false", "cause", "transmission", "treatment")]

lkup3 <- read.csv("./Data/fake_web_tweets.csv")
lkup3$misinformation <- 1 
lkup3$not_true_not_false <- NA
lkup3$false <- NA
lkup3$cause <- NA
lkup3$transmission <- NA
lkup3$treatment <- NA
lkup3 <- lkup3[,c("id", "misinformation", "not_true_not_false", "false", "cause", "transmission", "treatment")]

lkup4 <- read.csv("./Data/misinfo_user_tweets.csv")
lkup4$misinformation <- 1 
lkup4$not_true_not_false <- NA
lkup4$false <- NA
lkup4$cause <- NA
lkup4$transmission <- NA
lkup4$treatment <- NA
lkup4 <- lkup4[,c("id", "misinformation", "not_true_not_false", "false", "cause", "transmission", "treatment")]

lkup5a <- read.csv("./Data/lookup_who_terms_tweets.csv")
lkup5a <- lkup5a[lkup5a$cause == 1 | lkup5a$transmission == 1 | lkup5a$treatment == 1,] # Select only tweets with matches
lkup5a["X"] <- NULL # Drop variable as not needed
lkup5a$misinformation <- 1 
lkup5a$not_true_not_false <- NA
lkup5a$false <- NA

lkup5b <- read.csv("./Data/lookup_who_terms_tweets_lockdown.csv")
lkup5b <- lkup5b[lkup5b$cause == 1 | lkup5b$transmission == 1 | lkup5b$treatment == 1,]
lkup5b["X"] <- NULL
lkup5b$misinformation <- 1 
lkup5b$not_true_not_false <- NA
lkup5b$false <- NA

# Combine into one big happy list
lkup <- rbind(lkup1, lkup2, lkup3, lkup4, lkup5a, lkup5b) # Join together
write.csv(lkup, "./Data/lookup_all_misinformation.csv") # Save
rm(lkup1, lkup2, lkup3, lkup4, lkup5a, lkup5b) # Tidy

# Subset tweets that match ids
all_months <- all_months[,c("id", "rt_tweet_id", "qt_tweet_id")] # Subset required variables to make quicker

lkup_rt <- merge(lkup, all_months, by.x = "id", by.y = "rt_tweet_id") # Match tweets onto lookup based on retweet ids
lkup_rt$id <- NULL # Tidy up variables
lkup_rt$qt_tweet_id <- NULL
names(lkup_rt)[names(lkup_rt) == "id.y"] <- "id"

lkup_qt <- merge(lkup, all_months, by.x = "id", by.y = "qt_tweet_id") # Match tweets onto lookup based on quoted retweet ids
lkup_qt$id <- NULL 
lkup_qt$rt_tweet_id <- NULL
names(lkup_qt)[names(lkup_qt) == "id.y"] <- "id"

snowball <- rbind(lkup_rt, lkup_qt) # Join back together

# Repeat process to get full cascade

# 1
lkup_rt <- merge(snowball, all_months, by.x = "id", by.y = "rt_tweet_id") 
lkup_rt$id <- NULL 
lkup_rt$qt_tweet_id <- NULL
names(lkup_rt)[names(lkup_rt) == "id.y"] <- "id"

lkup_qt <- merge(snowball, all_months, by.x = "id", by.y = "qt_tweet_id") 
lkup_qt$id <- NULL 
lkup_qt$rt_tweet_id <- NULL
names(lkup_qt)[names(lkup_qt) == "id.y"] <- "id"

hold <- rbind(lkup_rt, lkup_qt) 

# 2
lkup_rt <- merge(hold, all_months, by.x = "id", by.y = "rt_tweet_id") 
lkup_rt$id <- NULL 
lkup_rt$qt_tweet_id <- NULL
names(lkup_rt)[names(lkup_rt) == "id.y"] <- "id"

lkup_qt <- merge(hold, all_months, by.x = "id", by.y = "qt_tweet_id")
lkup_qt$id <- NULL 
lkup_qt$rt_tweet_id <- NULL
names(lkup_qt)[names(lkup_qt) == "id.y"] <- "id"

hold2 <- rbind(lkup_rt, lkup_qt) 

# Join all together
snowball <- rbind(snowball, hold, hold2)

# Save
write.csv(snowball, "./Data/lookup_snowballed_misinformation.csv") # Save
