# Load rvest library.
library("rvest")

# Define the URL from where you get your data.
url <- paste("http://www.imdb.com/search/title?genres=sci_fi&sort=user_rating,desc&title_type=feature&num_votes=25000,")

# Load the content from the IMDB page into the page variable.
page <- read_html(url)

# Find all HTML elements that have a class with the value "runtime"
movie_runtimes <- html_nodes(page, '.runtime')

# Take the content from those elements and put it into "runtimes"
runtimes <- html_text(movie_runtimes)

# Use a regular expression to find all occurrences where you have at least two numbers in a row.
# If you add the command "print(runtimes)" and run it, you will see that the first element it finds is one that only contains the word "Runtime".
# If you use a regular expression here, that element will be discarded.
runtimes_matches <- regexpr("\\d{2,}", runtimes)

# The previous command just found the locations where the number sequences were located.
# This command extracts them and puts them into runtimes_values.
runtimes_values <- regmatches(runtimes, runtimes_matches)

# Although the values in runtimes_values look like numbers, for R they are character strings.
# To do your calculations you first have to convert the values to numbers.
runtimes_numeric <- as.numeric(runtimes_values)

# Here you first calculate the average runtime (by using the mean function), then you round it (by using the round function).
# Note that if you nest functions, the innermost function gets execute first and R then moves to the outside.
# You can also write this line as two separate lines.
runtimes_final <- round(mean(runtimes_numeric))

# Create a text string using the defined elements which are separate by a space.
paste("Average Runtime:", runtimes_final, "minutes", sep = " ")