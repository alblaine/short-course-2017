#install.packages("tidyverse")
#install.packages("gutenbergr")
#install.packages("NLP")
#install.packages("tm")
install.packages("scales")
install.packages("stringr")
install.packages("tidytext")

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

# downloads full texts of each work
works <- gutenberg_download(work_ids, meta_fields = "title", strip=TRUE)

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

# creates a frequency table for each word in texts with a proportion variable

ungroup(tidy_poe)

frequency <-
  mutate(tidy_poe, author = "Edgar Allan Poe") %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Edgar Allan Poe`)

head(frequency, 30)

# creates bigrams

poe_bigrams <- works %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# finds most common bigrams by count
poe_bigrams %>%
  count(bigram, sort = TRUE)

# filters out stop words and recounts bigrams
bigrams_separated <- poe_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# searches for top bigrams based on input for word2
bigram_search <- bigrams_filtered %>%
  filter(word2 == "time") %>%
  count(word1, title, sort = TRUE)

bigram_search