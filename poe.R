#install.packages("tidyverse")
#install.packages("gutenbergr")
#install.packages("NLP")
#install.packages("tm")

library(tidyverse)
library(gutenbergr)
library(NLP)
library(tm)

# gets information about all poe works
poe <- gutenberg_works(author == "Poe, Edgar Allan") 

# stores all ids of works
work_ids <- poe[1]

# downloads full texts of each work
works <- gutenberg_download(work_ids, strip=TRUE)

# tokenizes the texts and takes out stop words
tidy_poe <- works %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# runs a word count and sorts highest first across all texts
count <- tidy_poe %>%
  count(word, sort = TRUE)

# creates a bar chart of most frequent words across all texts
tidy_poe %>%
  count(word, sort = TRUE) %>%
  filter(n > 400) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
