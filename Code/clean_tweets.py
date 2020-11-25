# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

# Load packages
import pandas as pd
import nltk
import csv
import re
pd.options.mode.chained_assignment = None


# Load data
tweets = pd.read_csv('../Data/twitter_covid_feb_small.csv', encoding='latin-1')


### Clean tweets ###


## Convert full text to string ##


# Some tweets are classified by Python as 'floats' so need to convert to string for cleaning process
tweets['full_text_str'] = tweets['full_text'].astype(str)


## Remove punctuation ##


# We are not interested in punctuation for analyses so replace them with a space
# tweets['full_text_letters'] = tweets['full_text'].apply(lambda x : re.sub(r'[^a-zA-Z0-9 ]',' ',str(x)))
# If want to get rid of numbers too use re.sub(r'[^a-zA-Z ]' - normally we would get rid of numbers as well but if we want to look at '5G' then need to leave them in

    
## Convert all words to lower case ##


# To normalise comparisons else Love and love are treated seperately (for upper case swicth to 'word.upper())
tweets['full_text_lower'] = tweets['full_text_str'].apply(  lambda x: ' '.join( [ word.lower() for word in x.split() ] ) )
del tweets['full_text_str']


## Remove stop words ##


# Remove common words such as 'a', 'the', 'on' that do not contribute to the meaning of texts through providing unncessary information
from nltk.corpus import stopwords
stop = stopwords.words("english") # Define stopwords
tweets['full_text_stop'] = tweets['full_text_lower'].apply(lambda x: ' '.join([word for word in x.split() if word not in (stop)])) # Remove from tweet
del tweets['full_text_lower']


## Replace abbreviations ##


# Convert terms such as OMG to Oh My God - I have also included RT as retweet or HT as hattip
# Code From: https://medium.com/nerd-stuff/python-script-to-turn-text-message-abbreviations-into-actual-phrases-d5db6f489222
def translator(user_string):
    user_string = user_string.split(" ")
    j = 0
    for _str in user_string:
        # File path which consists of Abbreviations.
        fileName = "./slang.txt"

        # File Access mode [Read Mode]
        with open(fileName, "r") as myCSVfile:
            # Reading file as CSV with delimiter as "=", so that abbreviation are stored in row[0] and phrases in row[1]
            dataFromFile = csv.reader(myCSVfile, delimiter="=")
            # Removing Special Characters.
            _str = re.sub('[^a-zA-Z0-9]+', '', _str)
            for row in dataFromFile:
                # Check if selected word matches short forms[LHS] in text file.
                if _str.upper() == row[0]:
                    # If match found replace it with its appropriate phrase in text file.
                    user_string[j] = row[1]
            myCSVfile.close()
        j = j + 1
    return ' '.join(user_string)

tweets['full_text_abbr'] = tweets['full_text_stop'].apply(lambda x:  translator(x)  ) 
del tweets['full_text_stop']


## Normalising language ##


# Terms may be used with different tenses which ae currently being treated seperately. We have two options:

# 1. Stemming
# Normalise language by converting terms to their base/root (i.e. removes -ing, -ed, -s etc) e.g. waiting becomes wait
# Pros: computationally efficient
# Cons: root terms may be less obvious e.g. loving becomes lov 

#ps = PorterStemmer()
#tweets['Step3_SentimentText'] = tweets['full_text_stop'].apply(lambda x: ' '.join([ps.stem(word) for word in x.split() ]))

# 2. Lemmatization
# Convert terms to their root dictionary form (or lemma) e.g. runs, running and ran are each forms of run
# Pros: greater context to root terms as uses valid words
# Cons: requires greater memory to run, does not always get to root word

# We will go with Lemmatization as more useful in interpretation of words

# nltk.download() # To install WordNet corpora
from nltk.stem.wordnet import WordNetLemmatizer
lmtzr = WordNetLemmatizer()
tweets['full_text_cleaned'] = tweets['full_text_abbr'].apply(lambda x: ' '.join([lmtzr.lemmatize(word,'v') for word in x.split() ]))
del tweets['full_text_abbr']


## Parts of speech tagging ##


# Define structure of terms as nouns, pronouns, verbs etc
#tweets['full_text_pos'] = tweets['full_text_cleaned'].apply(lambda x: nltk.pos_tag(nltk.word_tokenize(x)))


## Tokenise data ##


## Code breaks up tweets into seperate words which is neccessary to analyse and identify terms
#from nltk.tokenize import word_tokenize
#tokens = tweets.full_text_cleaned.apply(word_tokenize)


## Save ##

# Save as csv file
tweets.to_csv('../Data/twitter_covid_feb_small_cleaned.csv')
