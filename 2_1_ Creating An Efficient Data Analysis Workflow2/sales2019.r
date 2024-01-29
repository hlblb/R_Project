
# title: "Guided Project: Creating An Efficient Data Analysis Workflow, Part 2"

library(tidyverse)
library(lubridate)

# 1. Data Exploration
# How big is the dataset?
data <- read_csv("sales2019.csv")
head(data)
colnames(data)
dim(data)

# Types of each of the columns
for (i in colnames(data)){
 paste("Column", i, ":", mode(data[[i]])) %>% print
}

# missig data
for (i in colnames(data)){
 paste("Column", i, ":", sum(is.na(data[[i]]))) %>% print
}


# 2. Handling Missing Data

# Remove all rows in the dataset that have an NA value 
# for the user_submitted_review column
new_data <- data %>% filter(!is.na(user_submitted_review))
head(new_data)
dim(data) # 5000,5
dim(new_data) # 4115,5

## Using the remaining rows that have data,
## calculate the average number of books purchased on an order.

avg_purchased <- mean(new_data$total_purchased, na.rm=TRUE)
avg_purchased

## Fill all of the missing values in total_purchased with the average value
## you calculated in step 2. We can do this through the following:

new_data <- data %>% 
    mutate(
        new_total_purchased = if_else(is.na(total_purchased),
                                               avg_purchased, 
                                               total_purchased)
    )
# No missing value
sum(is.na(new_data$new_total_purchased))


# 3. Processing Review Data
# Examine the unique sentences that are present in in user_submitted_review
table(new_data$user_submitted_review)

# Create a function that takes in a sentence (think: a value from user_submitted_review) # nolint
# and returns a value indicating if the review is positive or not.

positive <- function(review){
    positve_review = case_when(
        str_detect(review,"Awesome") ~ TRUE, 
        str_detect(review,"OK") ~ TRUE,
        str_detect(review,"a lot") ~ TRUE,
        str_detect(review,"Never") ~ TRUE,
        TRUE ~ FALSE
    )
}

# Create a new column in the dataset that indicates 
#whether or not the review in a given row is positive or not.
head(new_data)

new_data <- new_data %>% select(-total_purchased)
new_data <- new_data %>% mutate(
    positive_review = unlist(map(user_submitted_review,positive))
    )

head(new_data)

# 4. Comparing Book Sales Between Pre- and Post-Program Sales
# Perform the proper conversion of the date column, 
# so that it actually represents a date and time
library(lubridate)

new_data$date <- mdy(new_data$date)

# Create a new grouping column using the mutate() function 
# that will help distinguish between sales that happen 
# before July 1, 2019 and sales that happen after this date.

new_data <- new_data %>% mutate(
    before = if_else(date < ymd("2019-07-01"), TRUE, FALSE)
)

# Create a summary table that compares the number of books purchased 
# before July 1, 2019 to after

new_data %>% 
    group_by(before) %>%
    summarise(total = sum(new_total_purchased)) 

## does not seem to increase the sales


# 5. Comparing Book Sales Within Customer Type

# Perform the same analysis that you did in the last step 
# but add in the customer_type column to further subdivide the groups.
new_data %>% 
    group_by(before, customer_type) %>%
    summarise(total = sum(new_total_purchased)) 


# 6. Comparing Review Sentiment Between Pre- and Post-Program Sales

# Create another summary table that compares 
# the number of positive reviews before and after July 1, 2019

new_data %>%
    group_by(before) %>%
    summarize(total = sum(positive_review))
