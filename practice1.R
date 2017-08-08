# Practice Sentiment Analysis. This practice exercise corresponds to the sentiment.R file. Use that file as an example.
# The sample file are recent tweets about the topic 'back to school' on Twitter
# Tweets were harvested using this app: https://twitter-sentiment-csv.herokuapp.com/

# 1. You should have the libraries already installed for this 

# 2. Load the libraries: 

library(tidytext)
library(tidyverse)
library(RSentiment)

# 3. Load back-to-school tweets file

tweets_file <- read_csv("school.csv")

# 4. Get tweets from the text column

tweets <- 

# 5. Run some functions to clean the data

tweets <-               # uses iconv function (base package) to strip non-ASCII characters from tweets.

tweets <-               # converts tweets to lower case

tweets <-               # strips punctuation


# 6. Calculate the sentiment for each tweet.
  
sentiments <-           # calculate_sentiment() function is from the RSentiment package

  
# 7. Make a data frame of the tweets in one column, and the sentiments in the next column. Use data.frame() function. 

tweet_sents <-
  

# 8. Write the data to a CSV file
  
write.csv(          , file = "school-sentiments.csv")


# 9. Calculate total tweets in each category use calculate_total_presence_sentiment() function



# 10. Make a histogram of the sentiment ratings. x equals the counts of the sentiment variables.

______(tweet_sents) + ________aes(tweet_sents$________), stat="count")
