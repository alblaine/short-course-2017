# This script loads in a list of tweets about the weather and attempts to determine the sentiment for each tweet as one of the following:
# It classifies sentences into 6 categories: Positive, Negative, Very Positive, Very Negative Sarcasm and Neutral

install.packages("tidytext")
install.packages("tidyverse")
install.packages("RSentiment")
library(tidytext)
library(tidyverse)
library(RSentiment)

# read in Twitter data from a CSV file
tweets_file <- read_csv("weather-tweets.csv")

# get the tweet text only
tweets <- tweets_file$text

tweets <- iconv(tweets, to="ASCII", sub="") #strips non-ASCII characters from tweets

tweets <- tolower(tweets) #converts tweets to lower case

tweets <- gsub('[[:punct:]]', '', tweets) # takes out punctuation from the tweets

sentiments <- calculate_sentiment(tweets)  #function is from the RSentiment package

tweet_sents <- data.frame(sentiments) #makes a data frame from the tweets and sentiment ratings

tweet_sents  #prints out the data

# writes tweet_sents data frame to a file

write.csv(tweet_sents, file = "tweet-sentiments.csv")

# makes a bar chart of the sentiment ratings
ggplot(tweet_sents) + geom_histogram(aes(tweet_sents$sentiment), stat="count")
