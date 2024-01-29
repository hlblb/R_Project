library(readr)
library(dplyr)

setwd("C:/Users/sinks/OneDrive/바탕 화면/Project/R/21_Investigating COVID-19 Virus Trends")

### 1. Our analysis tries to provide an answer to this question: Which countries have had the highest number of positive cases against the number of tests? ### # 

### 2. Understanding Data ###
covid_df<-read_csv("covid19.csv")


dim(covid_df)
vector_cols <- colnames(covid_df)

head(covid_df)

library(tibble)
glimpse(covid_df)

### 3. Isolating the Rows We Need ###
## Filter the rows related to "All States" from the Province_State column and remove the Province_State column from covid_df dataframe.## # 
covid_df_all_states <- covid_df %>% filter(Province_State == "All States") %>% select(- Province_State)  # 
head(covid_df_all_states)

### 4. Isolating the Columns We Need
## Select the following column, related to the daily measures, from the covid_df_all_states: Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive.## # 

covid_df_all_states_daily <- covid_df_all_states %>% select(c(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)) # 
head(covid_df_all_states_daily)


### 5. Extracting the Top Ten Tested Cases Countries ###
## Write code to summarize the covid_df_all_states_daily dataframe by computing the sum of the number of tested, positive, active and hospitalized cases grouped by the Country_Region column.

covid_df_all_states_daily_sum <- covid_df_all_states_daily %>% group_by(Country_Region) %>% summarise(tested = sum(daily_tested), # 
                                                                    positive = sum(daily_positive), 
                                                                    active = sum(active),
                                                                    hospitalized = sum(hospitalizedCurr)) %>% arrange(desc(tested)) # 

covid_top_10 <- head(covid_df_all_states_daily_sum, 10)
covid_top_10

### 6. Identifying the Highest Positive Against Tested Cases ###

countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

names(positive_cases) <- countries
names(tested_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

positive_tested_top_3 = head(sort(positive_cases/tested_cases, decreasing =TRUE),3) 
positive_tested_top_3


### 7. Keeping relevant information ###

# Creating vectors
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)
covid_mat <- rbind(united_kingdom, united_states, turkey)
covid_mat
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized") 
covid_mat


### 8. Putting all together ###
question <- "Which countries have had the highest number of positive cases against the number of tests?" # 
answer <- c("Positive tested cases" = positive_tested_top_3)
answer
data_structure_list <- list(
  original = covid_df,
  allstates = covid_df_all_states,
  daily = covid_df_all_states_daily,
  top_10 = covid_top_10
)

covid_analysis_list <- list(question, answer, data_structure_list)
covid_analysis_list
