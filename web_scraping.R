# Word cloud code based on: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

# 1. Install all libraries required by this script.
#install.packages("rvest")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
#install.packages("RColorBrewer")

# 2. Import library for webscraping ("rvest")
library("rvest")

# 3. The following libraries are not required for webscraping, just for text processing and creating the word cloud that is based on the plot summaries.
library("tm") # Text mining
library("SnowballC") # Text stemming
library("wordcloud") # Word cloud generation
library("RColorBrewer") # Color palette creation

####################################################################################################
# Define variables

# 4. Select whether you want to look at the top 250 or bottom 100 movies on IMDb. Values are "top" or "bottom".
top_bottom <- "top"

# 5. Define number of individual movie pages from where you want to download plot summaries. The maximum value for top movies is 250 and for the bottom movies it's 100.
sample_count <- 50

# 6. File path to text file where you store the storylines.
filepath <- "/Users/mgwust/Desktop/r/storylines.txt"

# 7. File path for word cloud image file.
filepath_wordcloud <- "/Users/mgwust/Desktop/r/wordcloud.png"

####################################################################################################
# Get average ratings

# 8. Define URL for page you want to scrape.
url <- paste("http://www.imdb.com/chart/", top_bottom, sep = "")

# 9. Load HTML code into variable "page"
page <- read_html(url)

# 10. Get overall rating from each movie. This information is located in a <strong> element within an HTML element with the class
movie_ratings <- html_nodes(page, '.ratingColumn strong')

# 11. Extract the rating.
ratings_data <- html_text(movie_ratings)

# 12. Convert rating to a number so that we can calculate the average.
ratings_data <- as.numeric(ratings_data)

# Calculate the average rating.
avg_ratings <- mean(ratings_data)

# 13. Show average rating.
paste("Average Rating:", avg_ratings, sep = " ")

####################################################################################################
# Get average number of ratings

# 14. To get string that contains number of all votes per movie, select a <strong> element that's nested within an HTML element with the class "ratingColumn."
total_rankings_count_text <- html_nodes(page, ".ratingColumn strong")

# 15. The string is stored as the value an attribute of the <strong> element. Extract it. 
total_rankings_string <- html_attr(total_rankings_count_text, "title")

# 16. Use a regular expression to extract the position of the votes number from the total_rankings_count string. The expression looks for one or more digits, followed by a comma (both of which are optional), followed by one or more digits, followed by a comma, followed by three digits.
total_rankings_matches <- regexpr("(\\d{1,}\\,)*\\d{1,}\\,\\d{3}",total_rankings_string)

# 17. Extract the actual value from total_rankings_count.
rankings_count_value <- regmatches(total_rankings_string, total_rankings_matches)

# 18. Remove the comma from rankings_count_value.
rankings_count_value <- gsub("\\D", "", rankings_count_value)

# 19. Convert rankings_count_value to a number so that we can calculate the average.
rankings_count_value <- as.numeric(rankings_count_value)

# 20. Calculate the average vote count.
avg_counts <- mean(rankings_count_value)

# 21. Show average vount count.
paste("Average Vote Count:", avg_counts, sep = " ")

####################################################################################################
# Get brief storyline summaries for each movie

# 22. Select all a elements within element with class titleColumn that link to individual movie pages
all_links <- html_nodes(page, ".titleColumn a")

# 23. Select href attribute that contains link
all_links_url <- html_attr(all_links, "href")

# 24. Use a regular expression to extract the position of the movie id from the all_links_value string. The expression looks for the string "/title/tt" followed by seven digits.
all_links_matches <- regexpr("\\/title\\/tt\\d{7}", all_links_url)

# 25. Extract the actual value from all_links_matches.
all_links_value <- regmatches(all_links_url, all_links_matches)

# 26. Construct the request URLs for individual movie pages.
urls <- paste("http://www.imdb.com", all_links_value, sep = "")

# 27. Randomly select as many URLs as are defined in the sample_count variable.
urls_sample <- sample(urls, sample_count)

# 28. Go through the list of URLs in urls_sample and apply the get_storylines function to each of them.
storylines <- lapply(urls_sample, get_storylines)

# 29. Turn storylines list into vector
storylines <- unlist(storylines)

# 30. Save storylines to a text file and separate text strings from vectors with a newline character
write(storylines, file = filepath, sep = "\n")

####################################################################################################
# Process text and create word cloud

# 31. Read text from the file you just created.
text <- readLines(filepath)

# 32. Load data as a corpus.
documents <- Corpus(VectorSource(text))

# 33. Replace "-" with space.
documents <- tm_map(content_transformer(function(x) gsub("-", " ", x)))

# 34. Convert all text to lower case.
documents <- tm_map(documents, content_transformer(tolower))

# 35. Remove all numbers.
documents <- tm_map(documents, removeNumbers)

# 36. Remove punctuation.
documents <- tm_map(documents, removePunctuation)

# 37. Remove English stopwords.
documents <- tm_map(documents, removeWords, stopwords("english"))

# 38. Remove extra whitespace.
documents <- tm_map(documents, stripWhitespace)

# 39. Create a document-term matrix from data in documents variable. This lists how often each term appears in each of the documents.
document_term_matrix <- TermDocumentMatrix(documents)

# 40. Ensure that data is a matrix.
matrix <- as.matrix(document_term_matrix)

# 41. Sort data by overall frequency of words.
sorted <- sort(rowSums(matrix), decreasing = TRUE)

# 42. Load data into a data frame and assign column names.
word_list <- data.frame(word = names(sorted), freq = sorted)

# 43. Opens a graphic device for saving the word cloud as a .png file that will be saved in the location defined by the filename_wordcloud variable.
png(filename=filepath_wordcloud, width = 600, height = 600)

# 44. Generate word cloud. By changing the rot.per value you can change how many words are rotated by 90 degrees. 0 means all words are horizontal, 1 means all words are vertical.
# There are two arguments for the function brewer.pal which let you change the available color palette.
# The first one gives the maximum number of colors in the wordcloud, the second one selects which palette you want to use.
# Each palette can display at least 3 different colors, but the maximum number depends on which palette you use.
# The available palettes and their maximum number of colors are:
# - Accent (8)
# - Dark2 (8)
# - Paired (12)
# - Pastel1 (9)
# - Pastel2 (8)
# - Set1 (9)
# - Set2 (8)
# - Set3 (12)
wordcloud(words = word_list$word, freq = word_list$freq, min.freq = 1, max.words = 200, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

# 45. Close graphic device.
dev.off()

####################################################################################################
# Function for extracting storylines from individual movie pages

# 46. Define get_storylines function
get_storylines <- function(x) {
  
  # 47. Grab page from URL x.
  individual_page <- read_html(x)
  
  # 48. Select element with class summary_text within element with class plot_summary.
  storyline <- html_nodes(individual_page, ".plot_summary .summary_text")
  
  # 49. Extract text from this element and remove leading and trailing white spaces.
  storyline_text <- html_text(storyline, trim = TRUE)
  
  # 50. Remove the string "See full summary >>".
  storyline_text <- gsub("See full summary >>", "", storyline_text)
  
  # 51. Remove any new line characters.
  storyline_text <- gsub("\n", "", storyline_text)
  
  # 52. Return storyline text.
  storyline_text
}