#install.packages("tidyverse")
#install.packages("gutenbergr")
#install.packages("tm")
#install.packages("scales")
#install.packages("stringr")
#install.packages("tidytext")
#install.packages("igraph")
#install.packages("ggraph")
#install.packages("topicmodels")
#install.packages("NLP")
#install.packages("openNLP")

library(tidyverse)
library(gutenbergr)
library(tm)
library(scales)
library(stringr)
library(tidytext)
library(igraph)
library(ggraph)
library(topicmodels)
library(NLP)
library(openNLP)

# 1. Run this code to get information about all poe works, filtering duplicates
stoker <- gutenberg_works(author == "Stoker, Bram")


# 2. Print the information so you can see it. Type "stoker" in the console and hit enter.

stoker

# 3. Store all ids of works into a list so that you can then use that list to download works

work_ids <- stoker[1]

# 4. Type "work_ids" in the console and hit enter to print the list of ids

# 5. Download the full texts of each work, and include the title field in output

works <- gutenberg_download(work_ids, meta_fields = "title", strip=TRUE)

# 6. Type "works" in the console and hit enter to see what you downloaded.

# 7. Group the works by title so that you can use this grouping later in your analysis. group_by() is a function that is
# in the dplyr package (part of the tidyverse set of packages)

grouped_works <- works %>%
  group_by(title) 

#8. Tokenize the text of each of the works 

words <- grouped_works %>%
  unnest_tokens(word,text) 

#9. Type 'words' in the console and hit enter to see the output of what you just did


#10. Remove common stop words (a, an, the, but, or etc). 
data("stop_words")
words <- words %>%
  anti_join(stop_words)


# 11. Ungroup words by title and count most frequent words across all texts. 
# The ungroup() function removes connection to a particular book title.

count <- words %>%
  ungroup() %>%
  count(word, sort = TRUE)

# 11. Create a column chart of most frequent words across all texts where frequency (n) > 500

count %>%
  filter(n > 500) %>%  # applies a filter to the count
  mutate(word = reorder(word, n)) %>%   # reorders words based on the count #, greatest to least
  ggplot(aes(word, n)) + # plots the words and their counts in a column chart
  geom_col() + 
  xlab(NULL) + # sets x-axis label to blank
  coord_flip()  #flip coordinates so that the bars are horizontal rather than vertical


# 12. Change 500 in (n > 500) to a new number and see what happens to the graph. Re-run the code for #11


# 13. Now examine what words are most frequent in each document.

freq_words <- words %>%
  count(title, word, sort = TRUE) 

# 14. List total word counts (n) by title

total_words <- freq_words %>% 
  group_by(title) %>% 
  summarize(total = sum(n))

# 15. Create a joined table listing word frequencies and document word counts

words_table <- left_join(freq_words, total_words)

# 16. Calculate a tf-idf ratio for each word to find important terms by document
# Note: tf-idf helps find a document's distinguishing words (from other docs in the collection)

tf_idf_table <- words_table %>%
  bind_tf_idf(word, title, n)   # bind_tf_idf is a function from the tidytext package

# 17. Sort words so that highest-ranking tdf-idf terms show up first. 
# These are likely to be distinguishing terms, such as character names

distinct_words <- tf_idf_table %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# 18. Plot top 5 unique tf-idf terms per work. This uses dplyr and ggplot2 packages

plot_distinct <- distinct_words %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%  
  top_n(5) %>%  # selects top 3 entries in each group, in this case, by title
  ggplot(aes(word, tf_idf, fill = title)) +  #draws plot
  geom_col() +
  labs(x = NULL, y = "tf-idf") + 
  coord_flip() +
  facet_wrap(~title, nrow=5, scales="free")  # creates subplots based on title


# 19. Plot the top 15 unique terms for Dracula 

plot_dracula <- distinct_words %>%  # creates a variable where the plot will be stored
  filter(title =="Dracula") %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%  # sorts tf-idf from greatest to least
  top_n(15) %>%
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

# 20. See the plot by typing 'plot_dracula' in the console and pressing enter

plot_dracula

### Topic 2: N-grams / word pairs ### 

# 21. Create bigrams (word pairs) for all texts

bigrams <- works %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)


# 22. Type 'bigrams' into console and hit Enter to see output


# 23. Find most common bigrams by count

bigrams %>%
  count(bigram, sort = TRUE) # counts bigram pairs and sorts them by title 

# 24. Strip non-alpha characters (punctuation and dashes) from words

bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  mutate(word1 = str_extract(word1, "[a-z']+")) %>% 
  mutate(word2 = str_extract(word2, "[a-z']+"))

# 25. Filter out stop words from bigrams 

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# 26. Recount the bigrams

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)


#27. Make a network graph of the top bigrams

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2017)  # set is a starting number used to generate a random sequence of numbers - you can
# reproduce results if you set the seed to the same number each time. 
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# 27. Search for top bigrams based on input for word2, sorted by title
bigram_search <- bigrams_filtered %>%
  filter(word2 == "lucy") %>%
  count(word1, title, sort = TRUE)

# 28. Search for top bigrams overall based on word2 ("love")
bigram_no_titles <- bigrams_filtered %>%
  filter(word2 == "love") %>%
  count(word1, sort = TRUE)

# 29. Now search for another word in the texts by changing the word in line 175 from "love" to
# something else. Run the code to see what happens.


# 30. Search for a particular string ("death") in Dracula,
# extract those strings and write them to a .CSV file on your desktop
works %>% 
  filter(title=="Dracula", str_detect(text, "death")) %>% 
  select(text, title) %>%
  write.csv(.,file = "~/text-analysis-with-R/strings.csv")


### Topic 3: Topic Modeling Dracula ### 
## In this exercise, we are going to do some topic modeling on the chapters of Bram Stoker's Dracula

# 31. Filter 'works' to just include Dracula. 

dracula <- works %>%
  filter(title == "Dracula") 

# 32. Delete title page and table of contents by filtering out all text before line 157. 
# This step could be combined with previous step by adding a pipe operator %>% 

dracula <- dracula %>%
  mutate(linenumber = row_number()) %>%
  filter(linenumber > 156)


# 33. Now separate the text into chapters. This code looks for the word "Chapter" followed by Roman numerals and
# increments a counter

dracula <- dracula %>% 
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE))))


# 34. Tokenize text

chapter_words <- dracula %>%
  unnest_tokens(word, text)


# 35. Find word counts

word_counts <- chapter_words %>%
  anti_join(stop_words) %>%
  count(chapter, word, sort = TRUE) %>%
  ungroup() %>%
  unite(document, chapter)

# 36. Create a document term matrix on word counts using tidytext function 'cast_dtm()
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)                                


# 36. Run the Latent Dirichlet Allocation model on the document matrix.

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))


chapters_lda

# 37. Create a table where each word is listed with a probability of a term being generated from the topic 
# In this case, the probability that you'll find the term in a given chapter.
# beta = the probability of a term generated from a topic according to the multinomial model

chapter_topics <- tidy(chapters_lda, matrix = "beta")  
chapter_topics


# 38. Counts the top 5 terms with the highest probabilities by chapter.
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%  
  ungroup() %>%
  arrange(topic, -beta)  # arranges topics in descending order of highest-lowest beta

top_terms

# 39. Creates plots of topics

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +  # draws a bar chart
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +  # breaks chart into subplots, each with their own scales
  coord_flip()

# 40. The problem with the steps we just did is that the topics generated aren't useful. 
# They're heavily weighted toward character names and include lots of common terms (time, hand)
# Part-of-speech tagging can help us remove all non-nouns.


#### Part of Speech Tagging - DO NOT RUN this code in workshop. Skip to #41 ######
annotators <- list(sent_token = Maxent_Sent_Token_Annotator(),
                   word_token = Maxent_Word_Token_Annotator(),
                   pos_tag    = Maxent_POS_Tag_Annotator())

tagPOS <- function(x, ann = annotators) {
  s <- as.String(x)
  a2 <- annotate(s, list(ann$sent_token, ann$word_token))
  a3 <- annotate(s, ann$pos_tag, a2)
  a3w <- subset(a3, type == "word")
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)}

draculaTag <- tagPOS(dracula$text) # NOTE: tags dracula words with parts of speech tags

#write tagged text to a file
fileConn <- file("~/text-analysis-with-R/tagged2.txt")
writeLines(draculaTag$POStagged, fileConn)
close(fileConn)

######

# 41. Read in file with all Dracula words tagged with POS
tagged <- readLines("~/text-analysis-with-R/tagged2.txt")

# 42. Delete words that aren't nouns
nouns <- sapply(strsplit(tagged,"[[:punct:]]*/NN.?"),function(x) {res = sub("(^.*\\s)(\\w+$)", "\\2", x); res[!grepl("\\s",res)]} )

# 43. Make all words lower case and turn the list into a string of words.
nouns <- tolower(nouns) 

# 44. Make a list of names and common words to take out of the "nouns" 

stop_words <- c("mina", "count", "dracula", "van", "helsing", "lucy", "harker", "jonathan", "professor", "arthur", "dr", "time", "eyes", "hand")

nouns <- nouns[!nouns %in% stop_words]

nouns <-nouns[!nouns %in% stopwords()]

nouns

# 45. Turn the list of nouns into a data frame. Add a column for chapters

nouns_df <- data.frame(text=nouns, stringsAsFactors = F) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter", 
                                                 ignore_case = TRUE)))) %>%
  # count the number of times a word appears by chapter
  count(chapter, text, sort=TRUE) %>% 
  ungroup() %>%
  # replace the word chapter with 'document'
  unite(document, chapter)

# create a document term matrix from the nouns data frame

nouns_dtm <- nouns_df %>% cast_dtm(document, text, n)       


# run LDA algorithm on the dtm

nouns_lda <- LDA(nouns_dtm, k = 20, control = list(seed = 1234))

nouns_topics <- tidy(nouns_lda, matrix = "beta")

top_nouns <- nouns_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%  
  ungroup() %>%
  arrange(topic, -beta)  # arranges topics in descending order of highest-lowest beta

top_nouns

top_nouns %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +  # draws a bar chart
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +  # breaks chart into subplots, each with their own scales
  coord_flip()
