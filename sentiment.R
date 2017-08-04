# This script loads in a list of tweets about the weather and attempts to determine the sentiment for each tweet.
# It classifies sentences into 6 categories: Positive, Negative, Very Positive, Very Negative, Sarcasm, and Neutral

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

# 4. Get the tweet text only from the data you loaded in. 

tweets <- tweets_file$text  # tweets_file$text means the "text" variable of the tweets_file dataset

# 5. Run some functions to clean the data

tweets <- iconv(tweets, to="ASCII", sub="") # uses iconv function (base package) to strip non-ASCII characters from tweets.

tweets <- tolower(tweets) #converts tweets to lower case

tweets <- gsub('[[:punct:]]', '', tweets) # takes out punctuation from the tweets

tweets

# 6. Calculate the sentiment for each tweet.

sentiments <- calculate_sentiment(tweets)  # calculate_sentiment() function is from the RSentiment package

sentiments

# 7. Make a data frame of the tweets in one column, and the sentiments in the next column. 

tweet_sents <- data.frame(sentiments) # makes a data frame from the tweets and sentiment ratings using data.frame() function

tweet_sents 

# 8. Write the tweet_sents data frame to a new CSV file using write.csv() function from base R package

write.csv(tweet_sents, file = "tweet-sentiments.csv")

# 9. Sum sentiments by category

calculate_total_presence_sentiment(tweets)

# 10. Make a histogram of the sentiment ratings. x equals the counts of the sentiment variables

ggplot(tweet_sents) + geom_histogram(aes(tweet_sents$sentiment), stat="count")

# 11. The problem with the previous graph is that the X-axis categories were out of order. 
# Let's create a list of categories in the correct order:

x_labels <- factor(tweet_sents$sentiment, levels=c("Sarcasm", "Very Negative", "Negative", "Neutral", "Positive", "Very Positive"))

# 12. Re-make the plot with ordered categories 

ggplot(tweet_sents) + geom_histogram(aes(x_labels), stat="count")

