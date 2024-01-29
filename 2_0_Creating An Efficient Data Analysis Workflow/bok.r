
setwd("C:/Users/sinks/OneDrive/바탕 화면/Project/R/22_Creating An Efficient Data Analysis Workflow")
getwd()

# 1. Getting Familiar With The Data
library(readr)
library(dplyr)
data <- read_csv("book_reviews.csv")
glimpse(data)

# How big is the dataset?
dim(data)

# What are the column names?
colnames(data)

# What are the types of each of the columns?
for (i in colnames(data)) {
    print(mode(i))
}

# What are the unique values are present in each of the columns?
for (i in 1 : (dim(data)[2])) {
    print(unique(data[,i]))
}


# 2. Handling Missing Data

# Create a new copy of the dataset that removes all of the rows that have missing data.
new_data <- data %>% filter(!is.na(review))

dim(data)
dim(new_data)


# 3. Dealing With Inconsistent Labels
table(new_data$state)

new_data <- new_data %>% 
    mutate(
        state = case_when(
            state == "California" ~ "CA",
            state == "New York" ~ "NY",
            state == "Texas" ~ "TX",
            state == "Florida" ~ "FL",
            TRUE ~ state
        )
)
table(new_data$state)

# 4. Transforming The Review Data

new_data <- new_data %>%
    mutate(
        review = case_when(
            review == "Poor" ~ 1,
            review == "Fair" ~ 2,
            review == "Good" ~ 3,
            review == "Great" ~ 4,
            review == "Excellent" ~ 5
        ) 
    )
head(new_data)

new_data <- new_data %>%
    mutate(
        is_high_review = if_else(review>3, TRUE, FALSE)
    )

# 5. Analyzing The Data (Find the most profitable book)

new_data %>%
    group_by(book) %>%
    summarise(
        purchase = n()
    ) %>%
    arrange(-purchase)

## The Book "Fundamentals of R for Beginners is the most profitable books in terms of the number of purchase"