#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun  8 13:59:44 2020

@author: Mark Green
"""

# Define details of our account (add own details here)
API_KEY = ''
API_SECRET_KEY = ''
DEV_ENVIRONMENT_LABEL = ''
API_SCOPE = 'fullarchive'  # 'fullarchive' for full archive, '30day' for last 31 days

# Define search terms 
SEARCH_QUERY = '(covid-19 OR Corona OR Coronavirus OR "The virus" OR Covid OR Covid19 OR Rona OR C19 OR COV-19 OR Corona-19 OR CV19 OR CV-19 OR CV OR NCov OR 2019nCov OR SARS-CoV-2 OR Coronaoutbreak OR Coronaapocalypse OR Carona OR Codvid OR Coronavid OR Corono OR Coron OR Covit OR Curona OR Corrona OR Covd OR Korona OR koronavirus OR Corvid OR corvid-19 OR covid_19uk OR covid-19uk OR Briefing_COVID19 OR coronavirusuk OR COVID_19uk OR covid19uk OR CoronavirusBillUK OR UKCoronavirusBill OR uklockdown OR "Chinese virus" OR chinesevirus OR "Wuhan virus" OR wuhanvirus OR Boomerremover OR Covididiot OR Covididiots OR covidiot OR covidiots OR Kung Flu) lang:en (place_country:GB OR profile_country:GB)' # Max 1024 characters

# SEARCH_QUERY = 'from:snopes' # For specific Twitter users

RESULTS_PER_CALL = 500  # 100 for sandbox, 500 for paid tiers
FROM_DATE = '2020-03-02 00:00'  # format YYYY-MM-DD HH:MM (hour and minutes optional)
TO_DATE = '2020-03-10 00:00' # format YYYY-MM-DD HH:MM (hour and minutes optional)
# E.g. TO_DATE = 2020-02-14 00:00 starts at Feb 13 23:59:59 +0000 2020 back
# FROM_DATE = 2020-02-08 00:00 ends at Feb 08 00:00:00 +0000 2020
MAX_RESULTS = 100000000  # Number of Tweets you want to collect e.g. 1000000

# Last downloaded 1st March (~81k)

# Search 1: Covid-related terms

# General terms
# covid-19 OR Corona OR Coronavirus OR The virus OR Covid OR Covid19 OR Rona OR C19 OR COV-19 OR Corona-19 OR CV19 OR 
# CV-19 OR CV OR NCov OR 2019nCov OR SARS-CoV-2 OR Coronaoutbreak OR Coronaapocalypse

# Misspellings 
# Carona OR Codvid OR Coronavid OR Corono OR Coron OR Covit OR Curona OR Corrona OR Covd OR Korona OR koronavirus OR
# Corvid OR corvid-19 OR

# UK specific
# covid_19uk OR covid-19uk OR Briefing_COVID19 OR coronavirusuk OR COVID_19uk OR covid19uk OR CoronavirusBillUK
# UKCoronavirusBill OR uklockdown

# Derogatory terms or discrimination 
# Chinese virus OR chinesevirus OR Wuhan virus OR wuhanvirus OR Boomerremover OR Covididiot OR Kung Flu

# Search 2: Public Health / policy / treatment terms 

# Testing OR trace OR PPE OR ppeshortage OR Antibody OR antibodies OR Stay alert OR control the virus OR save lives OR stayalert OR
# Stay at home OR stayathome OR stayhome OR Lockdown OR uklockdown OR Quarantine OR quarentine OR quarantaine OR Isolation OR iso OR
# Self isolate OR Wash your hands OR washyourhands OR washurhands OR Social distancing OR socialdistancing OR social distance OR
# SocialDistancingNow OR Shielding OR sheltering OR Lockdown OR lockdown fatigue OR Mask OR n95 OR hydroxychloroquine OR
# clapforcarers

# Specific searches
# lang:en # Only English tweets
# (place_country:GB OR profile_country:GB) # Only UK tweets

# Define where Tweets should be saved
FILENAME = '../Data/twitter_covid_mar.jsonl'

# Print update for every X tweets downloaded
PRINT_AFTER_X = 500

# Define YAML with key details for accessing Twitter API
import yaml
config = dict(
    search_tweets_api=dict(
        account_type='premium',
        endpoint=f"https://api.twitter.com/1.1/tweets/search/fullarchive/datacollection.json",
        consumer_key=API_KEY,
        consumer_secret=API_SECRET_KEY
    )
)

with open('twitter_keys.yaml', 'w') as config_file:
    yaml.dump(config, config_file, default_flow_style=False)

## We can test searches in the sandbox environment without adding to monthly count here
# from searchtweets import collect_results, gen_rule_payload
# rule = gen_rule_payload("beyonce", results_per_call=100) # define search terms
# tweets = collect_results(rule, max_results=100) # Collect tweets
# [print(tweet.all_text, end='\n\n') for tweet in tweets[0:10]]; # Print
    
import json
from searchtweets import load_credentials, gen_rule_payload, ResultStream

# Define rules for premium search for streaming tweets
premium_search_args = load_credentials("twitter_keys.yaml",
                                       yaml_key="search_tweets_api",
                                       env_overwrite=False)

# Put together search terms and rules from earlier
rule = gen_rule_payload(SEARCH_QUERY,
                        results_per_call=RESULTS_PER_CALL,
                        from_date=FROM_DATE,
                        to_date=TO_DATE
                        )

# Stream tweets rather than download in one go
rs = ResultStream(rule_payload=rule,
                  max_results=MAX_RESULTS,
                  **premium_search_args)

# Access API and save each tweet as single line on JSON lines file
with open(FILENAME, 'a', encoding='utf-8') as f:
    n = 0
    for tweet in rs.stream():
        n += 1
        if n % PRINT_AFTER_X == 0:
            print('{0}: {1}'.format(str(n), tweet['created_at']))
        json.dump(tweet, f)
        f.write('\n')
print('done')
