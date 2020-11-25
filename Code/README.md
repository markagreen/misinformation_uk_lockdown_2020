# Code
All analytical code from accessing the Twitter API, cleaning the data, through to analysing the data and producing the outputs presented within the paper. Code run in a mixture of Python and R. Description of files below, divided by purpose of each file. The data refers to tweets collected within a larger project that here includes all tweets from 31st December 2019 to 10th March 2020 (tweets saved by month), and then the data used in the project saved as 'lockdown' or 'lockdown_mar2020' (different names used to refer to same data).

_Accessing Twitter data_

* _twitter_api.py_ - File used to access the Twitter Premium API and download tweets.
* _load_data.py_ - For processing the downloaded data into a format that can be loaded and maniupated for analysis (including to save space by dropping variables not required). File pulls out information about retweets as well.

_Data wrangling_

* _clean_tweets.py_ - Run first on files once processed using _load_data.py_. The file does some cleaning of the text information within tweets including removing punctuation, removing stop words (e.g. a, the, on - these do not contribute much to understanding what people are tweeting about), replacing abbreviations, and normalising language (lemmatization).
* _clean_tweets.R_ - Run second. Further processes data including identifying URLs in tweets and cleans text to remove special characters.
* _bots.R_ - File takes all twitter users present in the data and passes it through [tweetbotornot](https://github.com/mkearney/tweetbotornot) to estimate whether it was a bot or not.
* classify_misinformation.R_ - Classifies tweets by type of misinformation.

_Data analysis_

* _time_series_analysis.R_ - Code to run all of the time-series analyses using the data on counts by hour or minute of days.
* _topic_modelling.R_ - Code to run the topic modelling analyses to classify tweets identified as misinformation based on their content.
