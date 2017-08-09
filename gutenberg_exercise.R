library(tidyverse)
library(gutenbergr)
library(tidytext)

#### Fill in the blanks to practice writing R code. Use stoker.R Exercises 1-20 as a guide. #########################

# 1. Fill in the blanks to get information about an author's works in Project Gutenberg. Example: author == "Poe, Edgar Allan"
# Browse authors: http://www.gutenberg.org/browse/scores/top#authors-last7

_____ <- gutenberg_works(author == "_____")


# 2. Print the information so you can see it. Type the name of the variable you assigned in #1.

# 3. Store all ids of works into a list so that you can then use that list to download works

new_work_ids <- _____[1]

# 4. Type "work_ids" in the console and hit enter to print the list of ids

# 5. Download the full texts of each work using work_ids variable, and include the title field in output

new_works <- ______________(new_work_ids, meta_fields = "title", strip=TRUE) # use the gutenberg_download() function

# 6. Type "new_works" in the console and hit enter to see what you downloaded.

# 7. Group the works by title so that you can use this grouping later in your analysis. group_by() is a function that is
# in the dplyr package (part of the tidyverse set of packages)

new_grouped_works <- new_works %>%
  group_by(_______) 

# 8. Tokenize the text of each of the works using the unnest_tokens() function

new_words <- new_grouped_works %>%
  ________(word,text) 

# 9. Type 'words' in the console and hit enter to see the output of what you just did

# 10. Remove common stop words (a, an, the, but, or etc). Hint: stop_words (no quotes) 
# is the variable to plug into the anti_join function

data("stop_words")
new_words <- words %>%
  anti_join(_______)

new_words

# 11. Ungroup words by title and count most frequent words across all texts. The ungroup() function removes connection to a particular book title.

new_count <- new_words %>%
  ungroup() %>%
  _____(word, sort = TRUE)  # use the count() function

new_count

# 12. Create a column chart of most frequent words across all texts where frequency (n) > 500

new_graph_count <- new_count %>%
  filter(n > _____) %>%  # applies a filter to the count
  ______(word = reorder(word, n)) %>%   # use mutate() to create a new column "word" where the words are ordered as factors by n
  ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()  # uses ggplot library to create a column chart from filtered data

new_graph_count #prints out the graph


