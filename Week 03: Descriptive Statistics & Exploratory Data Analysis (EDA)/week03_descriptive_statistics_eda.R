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

# Load the Mall Customers dataset
data <- read.csv("Mall_Customers.csv")

# View the first few rows
head(data)

# Inspect the structure
str(data)

# Rename columns to make them easier to use in R
names(data) <- c("customer_id", "gender", "age", "income", "spending_score")

# Check updated column names
names(data)



############################################################
# 3. Basic Data Preparation for EDA
############################################################

# Convert gender to a categorical variable (factor)
data$gender <- as.factor(data$gender)

# Check for missing values
colSums(is.na(data))

# Remove rows with missing values if any exist
data <- na.omit(data)

# View the cleaned data
head(data)



############################################################
# 4. Summary Statistics
############################################################

# Overall summary of all variables
summary(data)

# Mean spending score
mean(data$spending_score)

# Median spending score
median(data$spending_score)

# Standard deviation of spending score
sd(data$spending_score)

# Minimum and maximum spending score
min(data$spending_score)
max(data$spending_score)

# Summary statistics for selected numeric variables
data %>%
  summarize(
    avg_age = mean(age),
    avg_income = mean(income),
    avg_spending_score = mean(spending_score)
  )



############################################################
# 5. Frequency Tables for Categorical Variables
############################################################

# Frequency count for gender
table(data$gender)

# Proportions for gender
prop.table(table(data$gender))



############################################################
# 6. Distribution Analysis
############################################################

# Histogram of spending score
hist(data$spending_score,
     main = "Distribution of Spending Score",
     xlab = "Spending Score",
     col = "lightblue",
     border = "white")

# Histogram of age
hist(data$age,
     main = "Distribution of Customer Age",
     xlab = "Age",
     col = "lightgreen",
     border = "white")

# Histogram of income
hist(data$income,
     main = "Distribution of Annual Income",
     xlab = "Annual Income (k$)",
     col = "lightpink",
     border = "white")

# Boxplot of income
boxplot(data$income,
        main = "Boxplot of Annual Income",
        ylab = "Annual Income (k$)",
        col = "lightgray")

# Boxplot of spending score
boxplot(data$spending_score,
        main = "Boxplot of Spending Score",
        ylab = "Spending Score",
        col = "orange")



############################################################
# 7. Correlation Analysis
############################################################

# Select numeric variables only
numeric_data <- data %>%
  select(age, income, spending_score)

# Correlation matrix
cor_matrix <- cor(numeric_data)

# Print the correlation matrix
cor_matrix

# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper")



############################################################
# 8. Data Visualization with ggplot2
############################################################

# Scatterplot: Age vs Spending Score
ggplot(data, aes(x = age, y = spending_score)) +
  geom_point(alpha = 0.6) +
  labs(title = "Customer Age vs Spending Score",
       x = "Age",
       y = "Spending Score")

# Scatterplot: Income vs Spending Score
ggplot(data, aes(x = income, y = spending_score)) +
  geom_point(alpha = 0.6) +
  labs(title = "Annual Income vs Spending Score",
       x = "Annual Income (k$)",
       y = "Spending Score")

# Scatterplot: Age vs Income
ggplot(data, aes(x = age, y = income)) +
  geom_point(alpha = 0.6) +
  labs(title = "Customer Age vs Annual Income",
       x = "Age",
       y = "Annual Income (k$)")

# Boxplot: Spending Score by Gender
ggplot(data, aes(x = gender, y = spending_score)) +
  geom_boxplot() +
  labs(title = "Spending Score by Gender",
       x = "Gender",
       y = "Spending Score")

# Boxplot: Income by Gender
ggplot(data, aes(x = gender, y = income)) +
  geom_boxplot() +
  labs(title = "Annual Income by Gender",
       x = "Gender",
       y = "Annual Income (k$)")

# Bar chart: Number of Customers by Gender
ggplot(data, aes(x = gender)) +
  geom_bar() +
  labs(title = "Customer Count by Gender",
       x = "Gender",
       y = "Count")



############################################################
# 9. Grouped Summaries to Identify Patterns
############################################################

# Average spending score by gender
data %>%
  group_by(gender) %>%
  summarize(
    avg_spending_score = mean(spending_score),
    avg_income = mean(income),
    avg_age = mean(age),
    customer_count = n()
  )



############################################################
# 10. Identifying Patterns in Customer Data
############################################################

# Example interpretation questions:
# - Do male and female customers differ in spending score?
# - Is spending score related to annual income?
# - Are younger customers associated with higher spending scores?
# - Are there possible outliers in income or spending score?

# Create age groups
data <- data %>%
  mutate(age_group = case_when(
    age < 30 ~ "Under 30",
    age >= 30 & age < 45 ~ "30-44",
    age >= 45 & age < 60 ~ "45-59",
    age >= 60 ~ "60+"
  ))

# Convert age_group to factor
data$age_group <- as.factor(data$age_group)

# Summary by age group
data %>%
  group_by(age_group) %>%
  summarize(
    avg_spending_score = mean(spending_score),
    avg_income = mean(income),
    count = n()
  )

# Visualization of spending score by age group
ggplot(data, aes(x = age_group, y = spending_score)) +
  geom_boxplot() +
  labs(title = "Spending Score by Age Group",
       x = "Age Group",
       y = "Spending Score")



############################################################
# 11. Optional: Save a Cleaned Version of the Dataset
############################################################

# write.csv(data, "mall_customers_week3_cleaned.csv", row.names = FALSE)



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
