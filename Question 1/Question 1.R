#Loading necessary libraries
library(dplyr)
library(tidyr)
library(lubridate)

#Load the dataset
data <- read.csv("C:\\Users\\saran\\Desktop\\Big Data 2\\Mid term\\Question1-Transposed Clean Data.csv")

#Inspecting the dataset to understand its format
head(data)
str(data)
colnames(data)

#First step: Cleaning the data
#before doing the conversion, in excel the date is mentioned as 'Rate', Hence I changed it to "Dates"
#Convert date column to proper Date format
data$Dates <- dmy(data$Dates)

#Display the range of dates to check for proper formatting
print(range(data$Dates, na.rm = TRUE))  # Show the minimum and maximum dates in the dataset


#Rename a column name to 'Prime rate'
data <- data %>%
  rename(`Prime rate` = Chartered.bank.administered.interest.rates...Prime.rate.1)

#Converting relevant numeric columns (e.g., bank rates) to numeric types if they are not
#If there are non-numeric values like 'NA', '..', 't', replace them with NA, and convert to numeric
data$Bank.rate <- as.numeric(data$Bank.rate)
data$`Prime rate` <- as.numeric(data$`Prime rate`)

#Remove unnecessary columns
data <- data %>%
  select('Dates', 'Bank.rate', 'Prime rate')  #Keeping only the columns required for the question

#viewing the selected data
head(data)

#Second step: Starting Question 1a
#transforming multiple columns into key-value pairs for Bank rate and Prime rate
data <- data %>%
  gather(key = "Rate_Type", value = "Rate_Value", 'Bank.rate', 'Prime rate')


#Finding the maximum date in the dataset
max_date <- max(data$Dates, na.rm = TRUE)
print(max_date)

#Calculating the date 5 years ago from the maximum date
date_5_years_ago <- max_date - years(5)
print(date_5_years_ago)

#Filter for the past 5 years based on the maximum date in the dataset
data_5_years <- data %>%
  filter(Dates >= date_5_years_ago)

# Show the minimum and maximum dates in the dataset after filterig
print(range(data_5_years$Dates, na.rm = TRUE))

# Checking the results of filtering
print(nrow(data_5_years))  #Number of rows in the filtered data
print(head(data_5_years))   #Print the first few rows to verify

#Calculating 6-month averages for Bank rate and Prime rate
#First group by 6-month intervals and calculate the average for each rate type
average_rates <- data_5_years %>%
  filter(Rate_Type %in% c('Bank.rate', 'Prime rate')) %>%  # Rate_Type column exists with Bank and Prime rates
  mutate(Period = floor_date(Dates, "6 months")) %>%        # Creating 6-month periods
  group_by(Period, Rate_Type) %>%
  summarise(Average_Rate = mean(Rate_Value, na.rm = TRUE))      # Calculating average rates for each period

#View the resulting data
print(average_rates)

#Third step: Question 1b
#Calculating the difference in days between consecutive rate changes for each rate type
rate_change_period <- data_5_years %>%
  group_by(Rate_Type) %>%
  arrange(Dates) %>%
  mutate(Days_Between_Changes = as.numeric(difftime(Dates, lag(Dates), units = "days")))  # Calculate difference between dates

#Calculate the average rate change period in days
average_days_change_period <- rate_change_period %>%
  group_by(Rate_Type) %>%
  summarise(Average_Days_Between = mean(Days_Between_Changes, na.rm = TRUE))

#View the resulting dataframe
print(average_days_change_period)

#Fourth Step: Question 1c
#Filtering out the Bank rate entries
bank_rate_data <- data %>%
  filter(Rate_Type == 'Bank.rate')

#Count the frequency of each unique bank rate
bank_rate_frequency <- bank_rate_data %>%
  group_by(Rate_Value) %>%
  summarise(Frequency = n())

#Viewing the frequency
print(bank_rate_frequency)

#Fifth step: Question 1d
#Arranging the data by the Frequency column in descending order
bank_rate_frequency_sorted <- bank_rate_frequency %>%
  arrange(desc(Frequency))

#Viewing the sorted frequency
print(bank_rate_frequency_sorted)

