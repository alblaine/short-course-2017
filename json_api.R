# 1. Import libraries for making API requests ("httr") and parsing JSON ("jsonlite")
library("httr")
library("jsonlite")

# 2. Define variables for API request, in this case latitude and longitude for Raleigh
latitude <- "35.7796"
longitude <- "-78.6382"

# 3. Construct API request URL: https://api.weather.gov/points/35.7796,-78.6382/forecast
url <- paste("https://api.weather.gov/points/",latitude,",",longitude, "/forecast", sep = "")

# 4. Getting data from API
result <- GET(url)

# 5. "result" contains information we don't need. Select only data from the "content" section.
result_data <- result$content

# 6. Data is returned as binary data. Convert it to characters.
result_data <- rawToChar(result_data)

# 7. Parse JSON data to an R list.
data <- fromJSON(result_data)

# 8. Select actual weather data and assign it to weather_data variable.
weather_data <- data[["properties"]]["periods"]

# 9. Select temperature column from weather_data and assign it to temps variable.
temps <- weather_data[["periods"]]["temperature"]

# 10. Convert temps data frame to a vector.
temps <- unlist(temps)

# 11. Calculate mean of temperatures and assign it to avg_temp variable.
avg_temp <- mean(temps)

# 12. Round avg_temp up or down.
avg_temp <- round(avg_temp)

# 13. Create text string with average 
report <- paste("Average Temperature:", avg_temp, "Degrees Fahrenheit", sep=" ")

# 14. Print report.
print(report)