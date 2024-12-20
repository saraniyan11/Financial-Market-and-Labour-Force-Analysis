#Loading necessary libraries
library(dplyr)
library(tidyr)

#Load the dataset
dataset <- read.csv("C:\\Users\\saran\\Desktop\\Big Data 2\\Mid term\\Question 2\\Question 2 Data.csv")

#Inspecting the dataset to understand its format
colnames(dataset)
tail(dataset)
head(dataset)
View(dataset)

str(dataset)

#Cleaning the dataset

#Renaming and dropping columns
dataset <- dataset %>%
  rename('September_2023' = X23.Sep) %>% 
  rename('August_2024' = X24.Aug) %>% 
  rename('September_2024' = X24.Sep) %>% 
  select(-Data.Type)

#Removing rows which contains NA or blank values
dataset <- na.omit(dataset)

#Converting relevant columns to numeric
dataset <- dataset %>%
  mutate(
    August_2024 = as.numeric(gsub(",", "", August_2024)),
    September_2024 = as.numeric(gsub(",", "", September_2024)),
    September_2023 = as.numeric(gsub(",", "", September_2023))
  )
View(dataset)

#Step 1: Question 2a
#Filter data for the required regions and labour characteristics
regions <- c("Ontario", "Alberta", "British Columbia", "Canada")
characteristics <- c("Population 6 7", "Full-time employment 10", "Part-time employment  11", "Unemployment 12")
filtered_dataset <- dataset %>%
  filter(Geography %in% regions & Labour.force.characteristics %in% characteristics)

#Pivot the data so that months are separate columns
pivot_data <- filtered_dataset %>%
  select(Geography, Labour.force.characteristics, September_2023, September_2024) %>%
  pivot_wider(names_from = Labour.force.characteristics, values_from = c(September_2023, September_2024))

#View pivot data to ensure it's structured correctly
head(pivot_data)
colnames(pivot_data)

#Renaming column name
pivot_data <- pivot_data %>%
  rename('September_2023_Population_6_7' = 'September_2023_Population 6 7') %>% 
  rename('September_2023_Full_time_employment_10' = 'September_2023_Full-time employment 10') %>% 
  rename('September_2023_Part_time_employment_11' = 'September_2023_Part-time employment  11') %>% 
  rename('September_2023_Unemployment_12' = 'September_2023_Unemployment 12') %>% 
  rename('September_2024_Population_6_7' = 'September_2024_Population 6 7') %>% 
  rename('September_2024_Full_time_employment_10' = 'September_2024_Full-time employment 10') %>% 
  rename('September_2024_Part_time_employment_11' = 'September_2024_Part-time employment  11') %>% 
  rename('September_2024_Unemployment_12' = 'September_2024_Unemployment 12')
  
colnames(pivot_data)

#Calculating growth rate for each region
growth_calculations <- pivot_data %>%
  mutate(
    Population_growth = (September_2024_Population_6_7 - September_2023_Population_6_7) / September_2023_Population_6_7 * 100,
    Full_time_growth = (September_2024_Full_time_employment_10 - September_2023_Full_time_employment_10) / September_2023_Full_time_employment_10 * 100,
    Part_time_growth = (September_2024_Part_time_employment_11 - September_2023_Part_time_employment_11) / September_2023_Part_time_employment_11 * 100,
    Unemployment_rate_growth = (September_2024_Unemployment_12 - September_2023_Unemployment_12) / September_2023_Unemployment_12 * 100
  ) %>%

  #Select relevant columns to display
  select(Geography, Population_growth, Full_time_growth, Part_time_growth, Unemployment_rate_growth)

#View the calculated growth for each region
print(growth_calculations)


#Step 2: question 2b

#Filter data for all regions and labour characteristics
regions_1 <- c("Ontario", "Alberta", "British Columbia", "Canada", "Newfoundland and Labrador",
             "Prince Edward Island", "Nova Scotia", "New Brunswick", "Quebec", "Manitoba", 
             "Saskatchewan")
characteristics_1 <- c("Part-time employment  11", "Labour force 8")
filtered_part_time_dataset <- dataset %>%
  filter(Geography %in% regions_1 & Labour.force.characteristics %in% characteristics_1)

#Pivot the data so that months are separate columns
pivot_part_time_data <- filtered_part_time_dataset %>%
  select(Geography, Labour.force.characteristics, September_2024) %>%
  pivot_wider(names_from = Labour.force.characteristics, values_from = c(September_2024))

#View pivot data to ensure it's structured correctly
head(pivot_part_time_data)
colnames(pivot_part_time_data)

#Renaming column
pivot_part_time_data <- pivot_part_time_data %>%
  rename('Labour_force_8' = 'Labour force 8') %>% 
  rename('Part_time_employment_11' = 'Part-time employment  11')

#Calculate Part-time employment rate for each province
part_time_rate <- pivot_part_time_data %>%
  mutate(Part_time_rates = (Part_time_employment_11 / (Labour_force_8)) * 100) %>%
  
  # Select relevant columns for output
  select(Geography, Part_time_rates) %>%
  # Arrange in descending order
  arrange(desc(Part_time_rates))

# View the results
print(part_time_rate)

#Step 3: question 2c (explanation is provided in the report)
#Filter data for all regions and labour characteristics
characteristics_2 <- c("Unemployment 12", "Employment 9", "Labour force 8")
filtered_analysis_df <- dataset %>%
  filter(Geography %in% regions_1 & Labour.force.characteristics %in% characteristics_2)

#Pivot the data frame
pivot_analysis_df <- filtered_analysis_df %>%
  select(Geography, Labour.force.characteristics, September_2024) %>%
  pivot_wider(names_from = Labour.force.characteristics, values_from = c(September_2024))

#View pivot data to ensure it's structured correctly
head(pivot_analysis_df)
colnames(pivot_analysis_df)

#renaming columns
pivot_analysis_df <- pivot_analysis_df %>% 
  rename('Employment_9' = 'Employment 9') %>% 
  rename('Labour_force_8' = 'Labour force 8') %>% 
  rename('Unemployment_12' = 'Unemployment 12')

#Analyzing employment & unemployment rates
analysis_df <- pivot_analysis_df %>%
  mutate(
    Employment_rate = (Employment_9/Labour_force_8) * 100,
    Unemployment_rate = (Unemployment_12/Labour_force_8) *100
  ) %>%
  # Select relevant columns for the analysis
  select(Geography, Employment_rate, Unemployment_rate) %>% 
  #For proper analysis
  arrange(desc(Employment_rate))

# View the analysis data frame
print(analysis_df)

