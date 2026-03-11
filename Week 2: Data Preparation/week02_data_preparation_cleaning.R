############################################################
# Marketing Analytics
# Week 2: Data Preparation and Data Cleaning
#
# Purpose of this script:
# In real-world analytics, data is rarely clean. Analysts
# spend a large portion of their time preparing data before
# conducting analysis.
#
# In this script we will learn:
# 1. Data types (categorical vs numerical)
# 2. Data cleaning
# 3. Handling missing values
# 4. Feature creation
# 5. Data transformations
# 6. Scaling variables
############################################################


############################################################
# 1. Load Required Packages
############################################################

library(tidyverse)


############################################################
# 2. Import Dataset
############################################################

# Load dataset
data <- read.csv("retail_customers_week1.csv")

# View first observations
head(data)


############################################################
# 3. Inspect Data Structure
############################################################

# Examine structure of dataset
str(data)

# View summary statistics
summary(data)

# Check column names
names(data)



############################################################
# 4. Understanding Data Types
############################################################

# In analytics, variables are typically:
# Numeric (continuous numbers)
# Categorical (labels or groups)

# Example numeric variable
class(data$age)

# Example categorical variable
class(data$gender)

# Convert gender to categorical (factor)

data$gender <- as.factor(data$gender)

data$region <- as.factor(data$region)

data$loyalty_member <- as.factor(data$loyalty_member)



############################################################
# 5. Identifying Missing Values
############################################################

# Count missing values in dataset

colSums(is.na(data))

# Percentage of missing values

colMeans(is.na(data)) * 100



############################################################
# 6. Handling Missing Data
############################################################

# Option 1: Remove rows with missing values

data_clean <- na.omit(data)

# Option 2: Replace missing values with mean

data$income[is.na(data$income)] <- mean(data$income, na.rm = TRUE)



############################################################
# 7. Detecting Data Problems
############################################################

# Check unrealistic values

summary(data$age)

# Example: remove negative values if present

data <- data %>%
  filter(age > 0)



############################################################
# 8. Feature Creation
############################################################

# Create a new variable: total purchases

data$total_purchases <- data$online_purchases + data$store_purchases

# Create spending per purchase

data$spending_per_purchase <- data$total_spending / data$total_purchases

# View updated dataset

head(data)



############################################################
# 9. Data Transformation
############################################################

# Sometimes variables are highly skewed
# Log transformation helps normalize distributions

data$log_spending <- log(data$total_spending + 1)



############################################################
# 10. Scaling Variables
############################################################

# Scaling standardizes variables so they have
# mean = 0 and standard deviation = 1

scaled_variables <- scale(data[,c("age","income","total_spending")])

head(scaled_variables)



############################################################
# 11. Simple Visualization After Cleaning
############################################################

ggplot(data, aes(x = age, y = total_spending)) +
  geom_point(color = "blue") +
  labs(title = "Customer Age vs Spending",
       x = "Age",
       y = "Total Spending")



############################################################
# End of Week 2 Script
#
# Students should now understand:
# - how to inspect datasets
# - how to identify variable types
# - how to clean data
# - how to handle missing values
# - how to create new variables
# - how to scale variables
############################################################
