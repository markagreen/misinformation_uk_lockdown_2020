######################
#### Clean tweets ####
######################

# Purpose: To clean tweets for analysis.

# Notes:
# The script cleans the column in the main files 'full_text_cleaned' - the raw tweet is maintained in column 'text'.
# The script goes after the python script of the same name.
# Only needs to be run once (done by MAG).

# Libraries
library(magrittr)
library(stringr)
library(dplyr)
library(tm)

### January ###

# Load data
jan <- read.csv("./Data/twitter_covid_jan_small_cleaned.csv", header = TRUE) 

# Keep only unique cases
jan <- distinct(jan, id, .keep_all = TRUE) 

# Clean terms
jan_cleaned <- jan %>% 
  # Convert to lowercase
  mutate(full_text_cleaned = full_text_cleaned %>% str_to_lower) %>% 
  # Remove unwanted characters
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '\\n')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '&amp')) %>% 
  # Remove websites
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'https://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'http://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'https')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'http')) %>% 
  # Remove hashtags
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '#')) %>% 
  # Remove hashtags if want to remove entire hashtag
  # mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '#[a-z,A-Z]*')) %>% 
  # Remove accounts
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '@[a-z,A-Z]*')) %>% 
  # Remove retweet text
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'rt [a-z,A-Z]*: ')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove(pattern = '^(rt)')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '\\_')) %>%
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'ht [a-z,A-Z]*: ')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove(pattern = '^(ht)'))
rm(jan)

# Replace accents to normalise full_text_cleaned
replacement.list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')
jan_cleaned %<>% 
  mutate(full_text_cleaned = chartr(old = names(replacement.list) %>% str_c(collapse = ''), 
                                    new = replacement.list %>% str_c(collapse = ''),
                                    x = full_text_cleaned))

# Store URLs
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+" # Define what we think a URL looks like so R can pick them out

jan_cleaned <- jan_cleaned %>% mutate(
  url1 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,1], # Select first URL in Tweet
  url2 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,2], # Second... (and so on)
  url3 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,3],
)

# Extract retweet or quoted information
jan_cleaned$rt_tweet_id <- NA # Tweet ID
jan_cleaned$rt_tweet_id <- qdapRegex::ex_between(jan_cleaned$retweeted_tweet, "'tweet_id': '", "', 'user_id':")

jan_cleaned$rt_username <- NA
jan_cleaned$rt_username <- qdapRegex::ex_between(jan_cleaned$retweeted_tweet, "'username': '", "', 'url':")

jan_cleaned$rt_url <- NA
jan_cleaned$rt_url <- qdapRegex::ex_between(jan_cleaned$retweeted_tweet, "'url': [{'url': '", "', 'expanded_url':")

jan_cleaned$rt_expanded_url <- NA
jan_cleaned$rt_expanded_url <- qdapRegex::ex_between(jan_cleaned$retweeted_tweet, "'expanded_url': '", "', 'display_url':")
jan_cleaned$rt_expanded_url <- gsub("c(https", "https", jan_cleaned$rt_expanded_url, fixed=T) # Some come with 'c(' at start where multiple

jan_cleaned$qt_tweet_id <- NA
jan_cleaned$qt_tweet_id <- qdapRegex::ex_between(jan_cleaned$quoted_tweet, "'tweet_id': '", "', 'user_id':")

jan_cleaned$qt_username <- NA
jan_cleaned$qt_username <- qdapRegex::ex_between(jan_cleaned$quoted_tweet, "'username': '", "', 'url':")

jan_cleaned$qt_url <- NA
jan_cleaned$qt_url <- qdapRegex::ex_between(jan_cleaned$quoted_tweet, "'url': [{'url': '", "', 'expanded_url':")

jan_cleaned$qt_expanded_url <- NA
jan_cleaned$qt_expanded_url <- qdapRegex::ex_between(jan_cleaned$quoted_tweet, "'expanded_url': '", "', 'display_url':")
jan_cleaned$qt_expanded_url <- gsub("c(https", "https", jan_cleaned$qt_expanded_url, fixed=T) # Some come with 'c(' at start where multiple URLs

# Extract website from expanding URLs (can't do that from tweets as all t.co)
jan_cleaned$rt_website <- NA
jan_cleaned$rt_website <- qdapRegex::ex_between(jan_cleaned$rt_expanded_url, "https://", "/")
jan_cleaned$rt_website <- as.character(jan_cleaned$rt_website) # Else is a list

jan_cleaned$qt_website <- NA
jan_cleaned$qt_website <- qdapRegex::ex_between(jan_cleaned$qt_expanded_url, "https://", "/")
jan_cleaned$qt_website <- as.character(jan_cleaned$qt_website) # Else is a list

# Save
write.csv(jan_cleaned, "./Data/twitter_covid_jan_small_cleaned.csv")
rm(jan_cleaned,replacement.list,url_pattern)


### February ###

# Load data
feb <- read.csv("./Data/twitter_covid_feb_small_cleaned.csv", header = TRUE)

# Keep only unique cases
feb <- distinct(feb, id, .keep_all = TRUE) 

feb_cleaned <- feb %>% 
  # Convert to lowercase
  mutate(full_text_cleaned = full_text_cleaned %>% str_to_lower) %>% 
  # Remove unwanted characters
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '\\n')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '&amp')) %>% 
  # Remove websites
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'https://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'http://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'https')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'http')) %>% 
  # Remove hashtags
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '#')) %>% 
  # Remove hashtags if want to remove entire hashtag
  # mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '#[a-z,A-Z]*')) %>% 
  # Remove accounts
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '@[a-z,A-Z]*')) %>% 
  # Remove retweet texts
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'rt [a-z,A-Z]*: ')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove(pattern = '^(rt)')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '\\_')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'ht [a-z,A-Z]*: ')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove(pattern = '^(ht)')) 

# Replace accents to normalise full_text_cleaned
replacement.list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')
rm(feb)

feb_cleaned %<>% 
  mutate(full_text_cleaned = chartr(old = names(replacement.list) %>% str_c(collapse = ''), 
                                    new = replacement.list %>% str_c(collapse = ''),
                                    x = full_text_cleaned))

# Store URLs
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+" # Define what we think a URL looks like so R can pick them out

feb_cleaned <- feb_cleaned %>% mutate(
  url1 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,1], # Select first URL in Tweet
  url2 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,2], # Second... (and so on)
  url3 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,3],
)

# Extract retweet or quoted information
feb_cleaned$rt_tweet_id <- NA # Tweet ID
feb_cleaned$rt_tweet_id <- qdapRegex::ex_between(feb_cleaned$retweeted_tweet, "'tweet_id': '", "', 'user_id':")

feb_cleaned$rt_username <- NA
feb_cleaned$rt_username <- qdapRegex::ex_between(feb_cleaned$retweeted_tweet, "'username': '", "', 'url':")

feb_cleaned$rt_url <- NA
feb_cleaned$rt_url <- qdapRegex::ex_between(feb_cleaned$retweeted_tweet, "'url': [{'url': '", "', 'expanded_url':")

feb_cleaned$rt_expanded_url <- NA
feb_cleaned$rt_expanded_url <- qdapRegex::ex_between(feb_cleaned$retweeted_tweet, "'expanded_url': '", "', 'display_url':")
feb_cleaned$rt_expanded_url <- gsub("c(https", "https", feb_cleaned$rt_expanded_url, fixed=T) # Some come with 'c(' at start where multiple URLs

feb_cleaned$qt_tweet_id <- NA
feb_cleaned$qt_tweet_id <- qdapRegex::ex_between(feb_cleaned$quoted_tweet, "'tweet_id': '", "', 'user_id':")

feb_cleaned$qt_username <- NA
feb_cleaned$qt_username <- qdapRegex::ex_between(feb_cleaned$quoted_tweet, "'username': '", "', 'url':")

feb_cleaned$qt_url <- NA
feb_cleaned$qt_url <- qdapRegex::ex_between(feb_cleaned$quoted_tweet, "'url': [{'url': '", "', 'expanded_url':")

feb_cleaned$qt_expanded_url <- NA
feb_cleaned$qt_expanded_url <- qdapRegex::ex_between(feb_cleaned$quoted_tweet, "'expanded_url': '", "', 'display_url':")
feb_cleaned$qt_expanded_url <- gsub("c(https", "https", feb_cleaned$qt_expanded_url, fixed=T) # Some come with 'c(' at start where multiple URLs

# Extract website from expanding URLs (can't do that from tweets as all t.co)
feb_cleaned$rt_website <- NA
feb_cleaned$rt_website <- qdapRegex::ex_between(feb_cleaned$rt_expanded_url, "https://", "/")
feb_cleaned$rt_website <- as.character(feb_cleaned$rt_website) # Else is a list

feb_cleaned$qt_website <- NA
feb_cleaned$qt_website <- qdapRegex::ex_between(feb_cleaned$qt_expanded_url, "https://", "/")
feb_cleaned$qt_website <- as.character(feb_cleaned$qt_website) # Else is a list

# Save
write.csv(feb_cleaned, "./Data/twitter_covid_feb_small_cleaned.csv")
rm(feb_cleaned,replacement.list,url_pattern)


### March ###

# Load data
mar <- read.csv("./Data/twitter_covid_mar_small_cleaned.csv", header = TRUE) 

# Keep only unique cases
mar <- distinct(mar, id, .keep_all = TRUE) 

# Clean terms
mar_cleaned <- mar %>% 
  # Convert to lowercase
  mutate(full_text_cleaned = full_text_cleaned %>% str_to_lower) %>% 
  # Remove unwanted characters
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '\\n')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '&amp')) %>% 
  # Remove websites
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'https://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'http://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'https')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'http')) %>% 
  # Remove hashtags
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '#')) %>% 
  # Remove hashtags if want to remove entire hashtag
  # mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '#[a-z,A-Z]*')) %>% 
  # Remove accounts
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '@[a-z,A-Z]*')) %>% 
  # Remove retweet text
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'rt [a-z,A-Z]*: ')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove(pattern = '^(rt)')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '\\_')) %>%
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'ht [a-z,A-Z]*: ')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove(pattern = '^(ht)'))
rm(mar)

# Replace accents to normalise full_text_cleaned
replacement.list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')
mar_cleaned %<>% 
  mutate(full_text_cleaned = chartr(old = names(replacement.list) %>% str_c(collapse = ''), 
                                    new = replacement.list %>% str_c(collapse = ''),
                                    x = full_text_cleaned))

# Store URLs
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+" # Define what we think a URL looks like so R can pick them out

mar_cleaned <- mar_cleaned %>% mutate(
  url1 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,1], # Select first URL in Tweet
  url2 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,2], # Second... (and so on)
  url3 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,3],
)

# Extract retweet or quoted information
mar_cleaned$rt_tweet_id <- NA # Tweet ID
mar_cleaned$rt_tweet_id <- qdapRegex::ex_between(mar_cleaned$retweeted_tweet, "'tweet_id': '", "', 'user_id':")

mar_cleaned$rt_username <- NA
mar_cleaned$rt_username <- qdapRegex::ex_between(mar_cleaned$retweeted_tweet, "'username': '", "', 'url':")

mar_cleaned$rt_url <- NA
mar_cleaned$rt_url <- qdapRegex::ex_between(mar_cleaned$retweeted_tweet, "'url': [{'url': '", "', 'expanded_url':")

mar_cleaned$rt_expanded_url <- NA
mar_cleaned$rt_expanded_url <- qdapRegex::ex_between(mar_cleaned$retweeted_tweet, "'expanded_url': '", "', 'display_url':")
mar_cleaned$rt_expanded_url <- gsub("c(https", "https", mar_cleaned$rt_expanded_url, fixed=T) # Some come with 'c(' at start where multiple URLs

mar_cleaned$qt_tweet_id <- NA
mar_cleaned$qt_tweet_id <- qdapRegex::ex_between(mar_cleaned$quoted_tweet, "'tweet_id': '", "', 'user_id':")

mar_cleaned$qt_username <- NA
mar_cleaned$qt_username <- qdapRegex::ex_between(mar_cleaned$quoted_tweet, "'username': '", "', 'url':")

mar_cleaned$qt_url <- NA
mar_cleaned$qt_url <- qdapRegex::ex_between(mar_cleaned$quoted_tweet, "'url': [{'url': '", "', 'expanded_url':")

mar_cleaned$qt_expanded_url <- NA
mar_cleaned$qt_expanded_url <- qdapRegex::ex_between(mar_cleaned$quoted_tweet, "'expanded_url': '", "', 'display_url':")
mar_cleaned$qt_expanded_url <- gsub("c(https", "https", mar_cleaned$qt_expanded_url, fixed=T) # Some come with 'c(' at start where multiple URLs

# Extract website from expanding URLs (can't do that from tweets as all t.co)
mar_cleaned$rt_website <- NA
mar_cleaned$rt_website <- qdapRegex::ex_between(mar_cleaned$rt_expanded_url, "https://", "/")
mar_cleaned$rt_website <- as.character(mar_cleaned$rt_website) # Else is a list

mar_cleaned$qt_website <- NA
mar_cleaned$qt_website <- qdapRegex::ex_between(mar_cleaned$qt_expanded_url, "https://", "/")
mar_cleaned$qt_website <- as.character(mar_cleaned$qt_website) # Else is a list

# Save
write.csv(mar_cleaned, "./Data/twitter_covid_mar_small_cleaned.csv")
rm(mar_cleaned,replacement.list,url_pattern)


### March lockdown ###

# Load data
lockdown <- read.csv("./Data/twitter_covid_lockdown_mar2020_small_cleaned.csv", header = TRUE) 

# Keep only unique cases
lockdown <- distinct(lockdown, id, .keep_all = TRUE) 

# Clean terms
lockdown_cleaned <- lockdown %>% 
  # Convert to lowercase
  mutate(full_text_cleaned = full_text_cleaned %>% str_to_lower) %>% 
  # Remove unwanted characters
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '\\n')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '&amp')) %>% 
  # Remove websites
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'https://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'http://t.co/[a-z,A-Z,0-9]*')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'https')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'http')) %>% 
  # Remove hashtags
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '#')) %>% 
  # Remove hashtags if want to remove entire hashtag
  # mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '#[a-z,A-Z]*')) %>% 
  # Remove accounts
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '@[a-z,A-Z]*')) %>% 
  # Remove retweet text
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'rt [a-z,A-Z]*: ')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove(pattern = '^(rt)')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = '\\_')) %>%
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove_all(pattern = 'ht [a-z,A-Z]*: ')) %>% 
  mutate(full_text_cleaned = full_text_cleaned %>% str_remove(pattern = '^(ht)'))
rm(lockdown)

# Replace accents to normalise full_text_cleaned
replacement.list <- list('á' = 'a', 'é' = 'e', 'í' = 'i', 'ó' = 'o', 'ú' = 'u')
lockdown_cleaned %<>% 
  mutate(full_text_cleaned = chartr(old = names(replacement.list) %>% str_c(collapse = ''), 
                                    new = replacement.list %>% str_c(collapse = ''),
                                    x = full_text_cleaned))

# Store URLs
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+" # Define what we think a URL looks like so R can pick them out

lockdown_cleaned <- lockdown_cleaned %>% mutate(
  url1 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,1], # Select first URL in Tweet
  url2 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,2], # Second... (and so on)
  url3 = str_extract_all(full_text, url_pattern, simplify = TRUE)[,3],
)

# Extract retweet or quoted information
lockdown_cleaned$rt_tweet_id <- NA # Tweet ID
lockdown_cleaned$rt_tweet_id <- qdapRegex::ex_between(lockdown_cleaned$retweeted_tweet, "'tweet_id': '", "', 'user_id':")

lockdown_cleaned$rt_username <- NA
lockdown_cleaned$rt_username <- qdapRegex::ex_between(lockdown_cleaned$retweeted_tweet, "'username': '", "', 'url':")
lockdown_cleaned$rt_expanded_url <- as.character(lockdown_cleaned$rt_expanded_url)
lockdown_cleaned$rt_expanded_url <- gsub("c(https", "https", lockdown_cleaned$rt_expanded_url, fixed=T) # Some come with 'c(' at start where multiple URLs

lockdown_cleaned$rt_url <- NA
lockdown_cleaned$rt_url <- qdapRegex::ex_between(lockdown_cleaned$retweeted_tweet, "'url': [{'url': '", "', 'expanded_url':")

lockdown_cleaned$rt_expanded_url <- NA
lockdown_cleaned$rt_expanded_url <- qdapRegex::ex_between(lockdown_cleaned$retweeted_tweet, "'expanded_url': '", "', 'display_url':")

lockdown_cleaned$qt_tweet_id <- NA
lockdown_cleaned$qt_tweet_id <- qdapRegex::ex_between(lockdown_cleaned$quoted_tweet, "'tweet_id': '", "', 'user_id':")

lockdown_cleaned$qt_username <- NA
lockdown_cleaned$qt_username <- qdapRegex::ex_between(lockdown_cleaned$quoted_tweet, "'username': '", "', 'url':")

lockdown_cleaned$qt_url <- NA
lockdown_cleaned$qt_url <- qdapRegex::ex_between(lockdown_cleaned$quoted_tweet, "'url': [{'url': '", "', 'expanded_url':")

lockdown_cleaned$qt_expanded_url <- NA
lockdown_cleaned$qt_expanded_url <- qdapRegex::ex_between(lockdown_cleaned$quoted_tweet, "'expanded_url': '", "', 'display_url':")
lockdown_cleaned$qt_expanded_url <- gsub("c(https", "https", lockdown_cleaned$qt_expanded_url, fixed=T) # Some come with 'c(' at start where multiple URLs

# Extract website from expanding URLs (can't do that from tweets as all t.co)
lockdown_cleaned$rt_website <- NA
lockdown_cleaned$rt_website <- qdapRegex::ex_between(lockdown_cleaned$rt_expanded_url, "https://", "/")
lockdown_cleaned$rt_website <- as.character(lockdown_cleaned$rt_website) # Else is a list

lockdown_cleaned$qt_website <- NA
lockdown_cleaned$qt_website <- qdapRegex::ex_between(lockdown_cleaned$qt_expanded_url, "https://", "/")
lockdown_cleaned$qt_website <- as.character(lockdown_cleaned$qt_website) # Else is a list

# Save
write.csv(lockdown_cleaned, "./Data/twitter_covid_lockdown_mar2020_small_cleaned.csv")
#rm(lockdown_cleaned,replacement.list,url_pattern)
