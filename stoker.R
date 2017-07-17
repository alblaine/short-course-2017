#install.packages("tidyverse")
#install.packages("gutenbergr")
#install.packages("NLP")
#install.packages("tm")
#install.packages("scales")
#install.packages("stringr")
#install.packages("tidytext")

library(tidyverse)
library(gutenbergr)
library(NLP)
library(tm)
library(scales)
library(stringr)
library(tidytext)

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

plot_dracula <- distinct_words %>%
  filter(title =="Dracula") %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%  # sorts tf-idf from greatest to least
  top_n(15) %>%
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

# 20. Create bigrams (word pairs) for all texts
bigrams <- works %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# finds most common bigrams by count
bigrams %>%
  count(bigram, sort = TRUE)

# filters out stop words, strips odd characters around words, and recounts bigrams
bigrams_separated <- poe_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  mutate(word1 = str_extract(word1, "[a-z']+")) %>%
  mutate(word2 = str_extract(word2, "[a-z']+"))

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# searches for top bigrams based on input for word2, sorted by title
bigram_search <- bigrams_filtered %>%
  filter(word2 == "heart") %>%
  count(word1, title, sort = TRUE)

# searches for top bigrams overall based on word1 ("heart")
bigram_no_titles <- bigrams_filtered %>%
  filter(word2 == "heart") %>%
  count(word1, sort = TRUE)

# searches for a particular string in originally downloaded text (stored in 'works')
# extracts those strings and writes them to a .CSV file on your desktop
works %>% 
  filter(str_detect(text, "heart")) %>% 
  select(text, title) %>%
  write.csv(.,file = "~/Desktop/poe.csv")

