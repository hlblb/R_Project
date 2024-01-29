## The goal of this guided project
# In this guided project, our task is to apply these techniques, as well as data analysis skills 
# you gained in earlier courses, to explore a select data set on forest fires.

#install required package
library(tidyverse)
data <- read_csv("forestfires.csv")
## 1. Investigate the data structure
head(data)

## 2. Data Processing
# Convert the month and day variable into a categorical variable, 
# and make sure that the months in the data are ordered correctly.

# check the current order
data %>% pull(month) %>% unique
data %>% pull(day) %>% unique

# set up the orders
month_order = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
day_order = c("sun","mon","tue","wed","thu","fri","sat")

data <- data %>% 
  mutate(month = factor(month, levels = month_order),
         day = factor(day,levels = day_order))

# check the updated order
data %>% pull(month) %>% unique


## 3. When Do Most Forest Fires Occur?

# Count the number of fires in each month and day
fires_by_month <- data %>%
                    group_by(month) %>%
                    summarize(fires_by_month = n())
fires_by_day <- data %>%
                    group_by(day) %>%
                    summarize(fires_by_day = n())


#Bar charts 
fires_by_month %>% 
  ggplot(aes(x = month,y = fires_by_month)) +
  geom_bar(stat = "identity") +
  labs(title = "Fires by Month",
       x = "Month",
       y = "Count")

fires_by_day %>%
  ggplot(aes(x = day,y = fires_by_day)) +
  geom_bar(stat = "identity") + 
  labs(title = "Fires by Day",
       x = "Day",
       y = "Count")
  

## 4. Plotting Other Variables Against Time

# FFMC
# DMC
# DC
# ISI
# temp
# RH
# wind
# rain

#Re-organize data set
new_data <- data %>%
              pivot_longer(cols = c(FFMC,
                 DMC,
                 DC,
                 ISI,
                 temp,
                 RH,
                 wind,
                 rain),
                 names_to = "variables",
                 values_to = "value")

# Plotting the data against time
new_data %>% 
  ggplot(aes(x = month,y=value))+
  geom_boxplot()+
  facet_wrap(vars(variables), scale = "free_y")+
  labs(title = "Variables changes over time",
       x = "Month",
       y = "Value")



## 5. Examining Forest Fire Severity
# Plotting the data against time

new_data %>%
    ggplot(aes(x = value,y = area))+
    geom_point()+
    facet_wrap(vars(variables),scale = "free_x")+
    labs(title = "Area over variables",
         x = "Value",
         y = "Burned Area")

## 6. Outlier Problems

# Distribution of area to figure out possible outlier
new_data %>%
  ggplot(aes(x = area))+
  geom_histogram()

# Remove a few large area cases (area > 300)
new_data %>%
  filter(area < 300) %>%
  ggplot(aes(x = value,y = area))+
  geom_point()+
  facet_wrap(vars(variables),scale = "free_x")+
  labs(title = "Area over variables",
       x = "Value",
       y = "Burned Area")


# Conclusion: Based on the visualizations, fires occur more in Aug and Sep than 
# other months. There is no significant difference found between days
# In terms of severity of fires using the size of area, the rain variable.
# One thing we can ask here is whether the size of area can be a good measure of severity 
# of fire or not.
  
