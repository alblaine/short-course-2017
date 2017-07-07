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

# gets information about all poe works, filtering duplicates
poe <- gutenberg_works(author == "Poe, Edgar Allan")

# stores all ids of works
work_ids <- poe[1]

# removes the non-literary work with id 25525 (The Raven Edition: Table of Contents and Index)
ids <- subset(work_ids, work_ids$gutenberg_id != 25525)

# downloads full texts of each work
works <- gutenberg_download(ids, meta_fields = "title", strip=TRUE)

# tokenizes the texts, groups them by title, and takes out stop words
tidy_poe <- works %>%
  group_by(title) %>%
  unnest_tokens(word,text) %>%
  anti_join(stop_words)

# runs a word count and sorts highest first across all texts
count <- tidy_poe %>%
  ungroup() %>%
  count(word, sort = TRUE)

# creates a bar chart of most frequent words across all texts
tidy_poe %>%
  ungroup() %>%
  count(word, sort = TRUE) %>%
  filter(n > 400) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# counts frequency of words per document
doc_words <- tidy_poe %>%
  count(title, word, sort = TRUE) %>%
  ungroup()

# lists total number of words (n) by title
total_words <- doc_words %>% 
  group_by(title) %>% 
  summarize(total = sum(n))

# creates a joined table listing word frequencies and document word counts
poe_words <- left_join(doc_words, total_words)

# creates a td_idf ratio for each word to find important terms by document
poe_words <- poe_words %>%
  bind_tf_idf(word, title, n)

# sorts so that high-ranking tdf-idf terms show up first
poe_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# creates bigrams (word pair associations)
poe_bigrams <- works %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# finds most common bigrams by count
poe_bigrams %>%
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


