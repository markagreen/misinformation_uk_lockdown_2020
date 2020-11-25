#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun  8 15:35:13 2020

@author: markagreen
"""

## To load the full JSONL file
##
#import json_lines
#
#def load_jsonl(file):
#    tweets = []
#    f=2
#    with open(file, 'rb') as f:
#        for tweet in json_lines.reader(f, broken=True):
#            tweets.append(tweet)
#    return (tweets)
#
#tweets = load_jsonl('../January/twitter_covid_jan_covidiots.jsonl') # Select file
#print(tweets[0]) # Print first tweet

# Load in data efficiently

import json_lines
import pprint

# To visualise JSONL structure better
def prettyprint(d, indent=0):
    for key, value in d.items():
        print('\t' * indent + str(key))
        if isinstance(value, dict):
            prettyprint(value, indent+1)
        else:
            print('\t' * (indent+1) + str(value))

# Loads in tweets one by one
def load_jsonl(file):
    tweets = [] # Create balnk file to read tweets into 
    with open(file, 'rb') as f:
        for tweet in json_lines.reader(f, broken=True): # For each tweet

            reduced_tweet = { # Store key details
                'created_at' : tweet['created_at'], # Time and date of tweet
                'id' : tweet['id_str'], # Unique ID of Tweet
                'username' : tweet['user']['screen_name'], # Username of Twitter profile
                'user_id' : tweet['user']['id_str'], # Unique ID for Twtter profile
                'text': tweet['text'] # Store text of tweet (140 characters max)
            }
            
            if 'extended_tweet' in tweet: # If tweet is more than 140 characters (Twitter seperates out old and current tweet lengths)
                reduced_tweet.update({'full_text':tweet['extended_tweet']['full_text']}) # Store full text (else cut off)
            elif 'retweeted_status' in tweet and 'extended_tweet' in tweet['retweeted_status']: # If a retweet and tweet more than 140 characters
                reduced_tweet.update({'full_text':tweet['retweeted_status']['extended_tweet']['full_text']}) # Store full text
            else: # Else if neither of previous two options, keep 140 characters text
                reduced_tweet.update({'full_text':tweet['text']})
            
            if 'derived' in tweet['user']: # If present in the users information
                if 'locations' in tweet['user']['derived']: # Store country
                    reduced_tweet.update({'country':tweet['user']['derived']['locations'][0]['country']})
#                else:
#                    reduced_tweet.update({}'country':''}) # If not present then store as missing
                
                if 'region' in tweet['user']['derived']['locations'][0]: # If present in the users information
                    reduced_tweet.update({'region':tweet['user']['derived']['locations'][0]['region']}) # Store region
#                else:
#                    reduced_tweet.update({'region':''}) # If not present then store as missing
            
            if 'retweeted_status' in tweet: # If a retweet (store as nested within same Tweet)
               reduced_tweet.update({'retweeted_user':{
                                       'user_id' : tweet['retweeted_status']['user']['id_str'], # Store user ID of retweeted user
                                       'username' : tweet['retweeted_status']['user']['screen_name']}}) # Store username
                
            #print("######################### ") # Prints progress (used for testing purposes to check code)
            #prettyprint(reduced_tweet)
                                
            tweets.append(reduced_tweet)
    return (tweets)

tweets = load_jsonl('../Data/twitter_covid_feb.jsonl') # Load specific file
print(tweets[10]) # Check loaded in fine

# Save
import json
with open('../Data/twitter_covid_feb_small.jsonl', 'w') as outfile:
    json.dump(tweets, outfile)

# Convert file to data frame
import pandas as pd
with open('../Data/twitter_covid_feb_small.jsonl', 'r') as f:
    data = json.load(f)
df = pd.DataFrame(data) # Converts to data frame
pd.set_option('display.max_columns', None) # So can view all columns
df.head(1) # Check has worked

# Save as csv file
df.to_csv('../Data/twitter_covid_feb_small.csv')
