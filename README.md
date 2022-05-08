# FinalExamR


library(httr)
library(rvest)

# TASK 1 
get_wiki_covid19_page <- function() {
  
  # Our target COVID-19 wiki page URL is: https://en.wikipedia.org/w/index.php?title=Template:COVID-19_testing_by_country  
  # Which has two parts: 
  # 1) base URL `https://en.wikipedia.org/w/index.php  
  # 2) URL parameter: `title=Template:COVID-19_testing_by_country`, seperated by question mark ?
  
  # Wiki page base
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  # You will need to create a List which has an element called `title` to specify which page you want to get from Wiki
  # in our case, it will be `Template:COVID-19_testing_by_country`
  query <- list(title="Template:COVID-19_testing_by_country")  
  # - Use the `GET` function in httr library with a `url` argument and a `query` argument to get a HTTP response
  data <- GET(wiki_base_url, query=query)  
  # Use the `return` function to return the response
  return(data)
}

# Call the get_wiki_covid19_page function and print the response
my_data <- get_wiki_covid19_page()
print(my_data)

# TASK 2
# Get the root html node from the http response in task 1 
root_node <- read_html(my_data)
print(root_node)

# Get the table node from the root html node
table <- html_node(root_node, "table")

# Read the table node and convert it into a data frame, and print the data frame for review
table_df <- html_table(table)
print(table_df)
class(table_df)

# TASK 3
# Print the summary of the data frame
summary(table_df)

#We have prepared a pre-processing function for you to conver the data frame but you can also try to write one by yourself
preprocess_covid_data_frame <- function(data_frame) {
  
  shape <- dim(data_frame)
  
  # Remove the World row
  data_frame<-data_frame[!(data_frame$`Country or region`=="World"),]
  # Remove the last row
  data_frame <- data_frame[1:172, ]
  
  # We dont need the Units and Ref columns, so can be removed
  data_frame["Ref."] <- NULL
  data_frame["Units[b]"] <- NULL
  
  # Renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  # Convert column data types
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
  
  return(data_frame)
}

# call `preprocess_covid_data_frame` function and assign it to a new data frame
new_data_frame <- preprocess_covid_data_frame(table_df)
print(new_data_frame)

# Print the summary of the processed data frame again
summary(new_data_frame)


# Export the data frame to a csv file
write.csv(new_data_frame, file="covid.csv")

# Read covid_data_frame_csv from the csv file
covid_data_frame_csv <- read.csv("covid.csv", header=TRUE, sep=",")

# Get the 5th to 10th rows, with two "country" "confirmed" columns
covid_data_frame_csv[5:10, c(1, 4)]

# Another way of retrieving this info
covid_data_frame_csv[5:10, c("country", "confirmed")]

# Get the total confirmed cases worldwide
covid_data_frame_csv[ , "confirmed"]

# Get the total tested cases worldwide
covid_data_frame_csv[ , "tested"]

# Get the positive ratio (confirmed / tested)
covid_data_frame_csv [, "confirmed.tested.ratio"]

# TASK 6

# Get the `country` column
country <- covid_data_frame_csv[ , "country"]
# Check its class (should be Factor)
class(country)
# Convert the country column into character so that you can easily sort them
country_char <- as.character(country)
# Sort the countries AtoZ
countryAZ <- sort(country_char, decreasing=FALSE)
# Sort the countries ZtoA
countryZA <- sort(country_char, decreasing=TRUE)
# Print the sorted ZtoA list
print(countryZA)

# TASK 7

# Use a regular expression `United.+` to find matches
countries_United <- grep("United.+", countryZA, value = TRUE)
# Print the matched country names
print(countries_United)

# TASK 8

# Select a subset (should be only one row) of data frame based on a selected country name and columns
Israel <- covid_data_frame_csv[78, c("country", "confirmed", "confirmed.population.ratio")]

# Select a subset (should be only one row) of data frame based on a selected country name and columns
Iran <- covid_data_frame_csv[75, c("country", "confirmed", "confirmed.population.ratio")]


print(Israel)
print(Iran)

# Task 9

# Use if-else statement
# if (check which confirmed.population value is greater) {
#    print()
# } else {
#    print()
# }
cases_population_ratio <- function(country_1,country_2) {
  if(country_1["confirmed.population.ratio"]>country_2["confirmed.population.ratio"]){
    print(country_1["country"])
    }
  else {
  print(country_2["country"])
  }
}

cases_population_ratio(Iran, Israel)


# Task 10

# Get a subset of any countries with `confirmed.population.ratio` less than the threshold

low_population_ratio <- function(my_data, threshold = 1) {
  subset <- my_data[1:172, c("country", "confirmed.population.ratio")]
  low_ratio <- subset["confirmed.population.ratio"] < 1
  return(subset[low_ratio, "country"])
}
