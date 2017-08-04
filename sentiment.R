# This script loads in a list of tweets about the weather and attempts to determine the sentiment for each tweet as one of the following:
# It classifies sentences into 6 categories: Positive, Negative, Very Positive, Very Negative Sarcasm and Neutral

# 1. Install each package if you don't already have it installed. Run each line of code.
install.packages("tidytext")
install.packages("tidyverse")
install.packages("RSentiment")

# 2. Now run each line of code to load the packages into your current R session
library(tidytext)
library(tidyverse)
library(RSentiment)

# 3. Read in Twitter data from a CSV file. The data is a list of tweets.
tweets_file <- read_csv("weather-tweets.csv")

# 4. Get the tweet text only from the data you loaded in
tweets <- tweets_file$text

tweets <- iconv(tweets, to="ASCII", sub="") #strips non-ASCII characters from tweets

tweets <- tolower(tweets) #converts tweets to lower case

tweets <- gsub('[[:punct:]]', '', tweets) # takes out punctuation from the tweets

sentiments <- calculate_sentiment(tweets)  #function is from the RSentiment package

tweet_sents <- data.frame(sentiments) #makes a data frame from the tweets and sentiment ratings

tweet_sents  #prints out the data

# writes tweet_sents data frame to a file

write.csv(tweet_sents, file = "tweet-sentiments.csv")

# sums sentiments by category

calculate_total_presence_sentiment(tweets)

# makes a histogram of the sentiment ratings
ggplot(tweet_sents) + geom_histogram(aes(tweet_sents$sentiment), stat="count")

# the problem with the previous graph is that the X-axis categories were out of order. 
# Let's create a list of categories in the correct order:

x_labels <- factor(tweet_sents$sentiment, levels=c("Sarcasm", "Very Negative", "Negative", "Neutral", "Positive", "Very Positive"))

# Re-make the plot with ordered categories 
ggplot(tweet_sents) + geom_histogram(aes(x_labels), stat="count")
