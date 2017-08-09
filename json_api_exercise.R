# 1. Import libraries for making API requests ("httr") and parsing JSON ("jsonlite")
library("httr")
library("jsonlite")

# 2. Define API request URL.
# This URL does not require additional parameters so you don't have to use paste() to construct a string.
# Data is for the 2015 Women's FIFA World Cup.
url <- "http://worldcup.sfg.io/teams/results"

# 3. Getting data from API
result <- GET(url)

# 4. "result" contains information we don't need. Select only data from the "content" section.
result_data <- result$content

# 5. Data is returned as binary data. Convert it to characters.
result_data <- rawToChar(result_data)

# 6. Parse JSON data to an R data frame.
fifa_data <- fromJSON(result_data)

# 7. Find the row in the data frame that has the maximum value in the "points" column.
# Put that data into a new data frame; this one only has a single row.
winner_data <- fifa_data[which.max(fifa_data$points), ]

# 8. Create text string that shows the winner of the 2015 Women's FIFA World Cup Champion, i.e., the team with the most points.
# You already have the winner information in the "winner_data" data frame.
# Now you just have to select the correct column ("country").
report <- paste("2015 Women's FIFA World Cup Champion:", winner_data$country, sep=" ")

# 9. Print the line.
print(report)