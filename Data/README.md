# Data
Folder contains required data to replicate analyses. Raw tweets cannot be openly shared as per the terms and conditions of Twitter. 

Description of files:

* _tweets_per_hour.csv_ - Data used to produce the results presented in Tables 2 and 3, and Appendix Figure A. Columns include 'hour' which is the date time by hour of day, 'misinformation' which is the total number of tweets identified as misinformation at the time, 'tweets' which is the total number of COVID-19 related tweets, 'bots' which is the total number of tweets estimated as from bots, 'intervention' which is a binary variable of whether the time was before (0) or after (1) the announcement, and finally 'bots_pc' which is the percentage of all tweets from bots at the time. 
* _tweets_per_hour_long.csv_ - As above but in long format. 'Hour' is the date time, 'type' is the variable, and 'count' refers to the value.
* _tweets_per_min.csv_ - Same data as in _tweets_per_hour.csv_ but for minutes of day (recorded in the column 'minute'). Same variables descriptions otherwise. Data used to calculate Appendix Tables A and B.
* _tweets_per_min_long.csv_ - As above but in long format (same format as hour file).
* _topic_models_fit.csv_ - Raw values of topic modelling model fit plotted in Appendix Figure B. Column 'topics' gives the number of topics within a model. The other columns refer to the values of four model fit evaluation statistics.
* _topics_per_hour.csv_ - Data used to produce the results in Table 4. Columns include 'hour' which is the date time by hour of day, 'topic' which is the topic (1 to 4), and 'n' which is the total number of tweets for each topic at the time.
