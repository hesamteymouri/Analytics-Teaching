############################################################
# Marketing Analytics
# Week 3: Descriptive Statistics and Exploratory Data Analysis
#
# Purpose of this script:
# This script introduces descriptive statistics and 
# exploratory data analysis (EDA) using customer data.
#
# Students will learn how to:
# 1. Generate summary statistics
# 2. Analyze variable distributions
# 3. Examine correlations among numeric variables
# 4. Visualize customer data
# 5. Identify basic patterns in customer behavior
############################################################


############################################################
# 1. Load Required Packages
############################################################

# Install packages if needed
# install.packages("tidyverse")
# install.packages("corrplot")

library(tidyverse)
library(corrplot)



############################################################
# 2. Import the Dataset
############################################################

# Load the Week 2 cleaned or messy dataset
# If using the messy dataset, students may first need to clean it

data <- read.csv("retail_customers_week2_messy.csv")

# View the first few rows
head(data)

# Inspect the structure
str(data)



############################################################
# 3. Basic Data Preparation for EDA
############################################################

# Convert categorical variables to factors
data$gender <- as.factor(data$gender)
data$region <- as.factor(data$region)
data$loyalty_member <- as.factor(data$loyalty_member)

# Remove rows with missing values for this week's analysis
# (In practice, other missing-data approaches are possible)
data <- na.omit(data)

# Remove impossible values
data <- data %>%
  filter(age > 0,
         income > 0)

# Create a useful derived variable
data$total_purchases <- data$online_purchases + data$store_purchases

# View the cleaned data
head(data)



############################################################
# 4. Summary Statistics
############################################################

# Overall summary of all variables
summary(data)

# Mean spending
mean(data$total_spending)

# Median spending
median(data$total_spending)

# Standard deviation of spending
sd(data$total_spending)

# Minimum and maximum spending
min(data$total_spending)
max(data$total_spending)

# Summary statistics for selected numeric variables
data %>%
  summarize(
    avg_age = mean(age),
    avg_income = mean(income),
    avg_spending = mean(total_spending),
    avg_visits = mean(visits_last_month),
    avg_total_purchases = mean(total_purchases)
  )



############################################################
# 5. Frequency Tables for Categorical Variables
############################################################

# Frequency count for gender
table(data$gender)

# Frequency count for region
table(data$region)

# Frequency count for loyalty membership
table(data$loyalty_member)

# Proportions
prop.table(table(data$loyalty_member))



############################################################
# 6. Distribution Analysis
############################################################

# Histogram of total spending
hist(data$total_spending,
     main = "Distribution of Total Spending",
     xlab = "Total Spending",
     col = "lightblue",
     border = "white")

# Histogram of age
hist(data$age,
     main = "Distribution of Customer Age",
     xlab = "Age",
     col = "lightgreen",
     border = "white")

# Boxplot of income
boxplot(data$income,
        main = "Boxplot of Income",
        ylab = "Income",
        col = "lightgray")

# Boxplot of spending
boxplot(data$total_spending,
        main = "Boxplot of Total Spending",
        ylab = "Total Spending",
        col = "orange")



############################################################
# 7. Correlation Analysis
############################################################

# Select numeric variables only
numeric_data <- data %>%
  select(age, income, visits_last_month,
         online_purchases, store_purchases,
         total_spending, total_purchases)

# Correlation matrix
cor_matrix <- cor(numeric_data)

# Print the correlation matrix
cor_matrix

# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper")



############################################################
# 8. Data Visualization with ggplot2
############################################################

# Scatterplot: Age vs Total Spending
ggplot(data, aes(x = age, y = total_spending)) +
  geom_point(alpha = 0.6) +
  labs(title = "Customer Age vs Total Spending",
       x = "Age",
       y = "Total Spending")

# Scatterplot: Income vs Total Spending
ggplot(data, aes(x = income, y = total_spending)) +
  geom_point(alpha = 0.6) +
  labs(title = "Income vs Total Spending",
       x = "Income",
       y = "Total Spending")

# Boxplot: Spending by Loyalty Membership
ggplot(data, aes(x = loyalty_member, y = total_spending)) +
  geom_boxplot() +
  labs(title = "Total Spending by Loyalty Membership",
       x = "Loyalty Member",
       y = "Total Spending")

# Boxplot: Spending by Gender
ggplot(data, aes(x = gender, y = total_spending)) +
  geom_boxplot() +
  labs(title = "Total Spending by Gender",
       x = "Gender",
       y = "Total Spending")

# Bar chart: Number of Customers by Region
ggplot(data, aes(x = region)) +
  geom_bar() +
  labs(title = "Customer Count by Region",
       x = "Region",
       y = "Count")



############################################################
# 9. Grouped Summaries to Identify Patterns
############################################################

# Average spending by loyalty membership
data %>%
  group_by(loyalty_member) %>%
  summarize(avg_spending = mean(total_spending),
            avg_income = mean(income),
            avg_purchases = mean(total_purchases))

# Average spending by gender
data %>%
  group_by(gender) %>%
  summarize(avg_spending = mean(total_spending),
            avg_income = mean(income),
            avg_visits = mean(visits_last_month))

# Average spending by region
data %>%
  group_by(region) %>%
  summarize(avg_spending = mean(total_spending),
            avg_income = mean(income),
            customer_count = n())



############################################################
# 10. Identifying Patterns in Customer Data
############################################################

# Example interpretation questions:
# - Do loyalty members spend more on average?
# - Is spending related to income?
# - Are some regions associated with higher average spending?
# - Are there possible outliers in income or spending?

# Compare spending across age groups
data <- data %>%
  mutate(age_group = case_when(
    age < 30 ~ "Under 30",
    age >= 30 & age < 45 ~ "30-44",
    age >= 45 & age < 60 ~ "45-59",
    age >= 60 ~ "60+"
  ))

data$age_group <- as.factor(data$age_group)

data %>%
  group_by(age_group) %>%
  summarize(avg_spending = mean(total_spending),
            avg_income = mean(income),
            count = n())

# Visualization of spending by age group
ggplot(data, aes(x = age_group, y = total_spending)) +
  geom_boxplot() +
  labs(title = "Total Spending by Age Group",
       x = "Age Group",
       y = "Total Spending")



############################################################
# 11. Optional: Save a Cleaned Version of the Dataset
############################################################

# write.csv(data, "retail_customers_week3_cleaned.csv", row.names = FALSE)



############################################################
# End of Week 3 Script
#
# Students should now be able to:
# - Generate descriptive statistics
# - Explore distributions
# - Examine correlations
# - Create visualizations for marketing data
# - Identify simple patterns in customer behavior
############################################################
