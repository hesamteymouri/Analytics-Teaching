############################################################
# Marketing Analytics
# Week 4: Exploratory Analysis for Customer Behavior and STP
#
# Purpose of this script:
# This script helps students explore customer behavior,
# understand customer heterogeneity, connect the findings
# to the STP framework, and prepare data for segmentation.
#
# Students will learn how to:
# 1. Explore behavioral differences across customers
# 2. Identify customer heterogeneity
# 3. Connect data patterns to Segmentation, Targeting,
#    and Positioning (STP)
# 4. Prepare variables for segmentation analysis
############################################################


############################################################
# 1. Load Required Packages
############################################################

# Install packages if needed
# install.packages("tidyverse")

library(tidyverse)



############################################################
# 2. Import the Dataset
############################################################

# Load the Mall Customers dataset
data <- read.csv("Mall_Customers.csv")

# View the first few rows
head(data)

# Inspect the structure
str(data)

# Rename variables to simpler names for easier coding
names(data) <- c("customer_id", "gender", "age", "income", "spending_score")

# Check updated variable names
names(data)



############################################################
# 3. Basic Data Preparation
############################################################

# Convert gender to a categorical variable
data$gender <- as.factor(data$gender)

# Remove missing values if any
data <- na.omit(data)

# Confirm dataset structure after cleaning
str(data)

# View summary statistics
summary(data)



############################################################
# 4. Behavioral Data Exploration
############################################################

# In this dataset, spending_score is the main behavioral variable.
# We can begin by exploring how it differs across customers.

# Distribution of spending score
hist(data$spending_score,
     main = "Distribution of Spending Score",
     xlab = "Spending Score",
     col = "lightblue",
     border = "white")

# Compare spending score by gender
ggplot(data, aes(x = gender, y = spending_score)) +
  geom_boxplot() +
  labs(title = "Spending Score by Gender",
       x = "Gender",
       y = "Spending Score")

# Explore income and spending behavior
ggplot(data, aes(x = income, y = spending_score)) +
  geom_point(alpha = 0.7) +
  labs(title = "Income and Spending Behavior",
       x = "Annual Income (k$)",
       y = "Spending Score")

# Explore age and spending behavior
ggplot(data, aes(x = age, y = spending_score)) +
  geom_point(alpha = 0.7) +
  labs(title = "Age and Spending Behavior",
       x = "Age",
       y = "Spending Score")



############################################################
# 5. Customer Heterogeneity
############################################################

# Customer heterogeneity means that customers differ
# from one another in meaningful ways.

# Create age groups to examine differences across segments
data <- data %>%
  mutate(age_group = case_when(
    age < 30 ~ "Under 30",
    age >= 30 & age < 45 ~ "30-44",
    age >= 45 & age < 60 ~ "45-59",
    age >= 60 ~ "60+"
  ))

data$age_group <- as.factor(data$age_group)

# Compare average spending across age groups
data %>%
  group_by(age_group) %>%
  summarize(
    avg_spending_score = mean(spending_score),
    avg_income = mean(income),
    customer_count = n()
  )

# Visualize spending score by age group
ggplot(data, aes(x = age_group, y = spending_score)) +
  geom_boxplot() +
  labs(title = "Spending Score by Age Group",
       x = "Age Group",
       y = "Spending Score")

# Compare average spending by gender
data %>%
  group_by(gender) %>%
  summarize(
    avg_spending_score = mean(spending_score),
    avg_income = mean(income),
    avg_age = mean(age),
    customer_count = n()
  )



############################################################
# 6. STP Framework: Segmentation, Targeting, Positioning
############################################################

# STP stands for:
# S = Segmentation
# T = Targeting
# P = Positioning

# In analytics, segmentation begins by identifying groups
# of customers who differ in characteristics or behavior.

# Example 1: Create simple spending categories
data <- data %>%
  mutate(spending_group = case_when(
    spending_score < 34 ~ "Low Spending",
    spending_score >= 34 & spending_score < 67 ~ "Medium Spending",
    spending_score >= 67 ~ "High Spending"
  ))

data$spending_group <- as.factor(data$spending_group)

# Examine customer counts across spending groups
table(data$spending_group)

# Compare average income and age across spending groups
data %>%
  group_by(spending_group) %>%
  summarize(
    avg_age = mean(age),
    avg_income = mean(income),
    customer_count = n()
  )

# Visualize income across spending groups
ggplot(data, aes(x = spending_group, y = income)) +
  geom_boxplot() +
  labs(title = "Income by Spending Group",
       x = "Spending Group",
       y = "Annual Income (k$)")

# Visualize age across spending groups
ggplot(data, aes(x = spending_group, y = age)) +
  geom_boxplot() +
  labs(title = "Age by Spending Group",
       x = "Spending Group",
       y = "Age")


# Interpretation idea:
# These groups are not final market segments yet,
# but they help us think about which customers may be
# more attractive to target and how a firm might position
# offerings differently for them.



############################################################
# 7. Preparing Data for Segmentation
############################################################

# Before segmentation, we typically:
# 1. Select relevant variables
# 2. Remove non-useful identifiers
# 3. Standardize numeric variables so they are on the same scale

# Select variables useful for segmentation
segmentation_data <- data %>%
  select(age, income, spending_score)

# View selected data
head(segmentation_data)

# Check summary statistics
summary(segmentation_data)

# Scale variables
segmentation_scaled <- scale(segmentation_data)

# View first few rows of scaled data
head(segmentation_scaled)

# Check the mean and standard deviation after scaling
apply(segmentation_scaled, 2, mean)
apply(segmentation_scaled, 2, sd)



############################################################
# 8. Visualizing Variables for Segmentation Readiness
############################################################

# Pairwise scatterplots help us see possible groupings
pairs(segmentation_data,
      main = "Scatterplot Matrix for Segmentation Variables")

# Scatterplot: income vs spending score
ggplot(data, aes(x = income, y = spending_score)) +
  geom_point(alpha = 0.7) +
  labs(title = "Income vs Spending Score",
       x = "Annual Income (k$)",
       y = "Spending Score")

# Scatterplot: age vs income
ggplot(data, aes(x = age, y = income)) +
  geom_point(alpha = 0.7) +
  labs(title = "Age vs Income",
       x = "Age",
       y = "Annual Income (k$)")

# Scatterplot: age vs spending score
ggplot(data, aes(x = age, y = spending_score)) +
  geom_point(alpha = 0.7) +
  labs(title = "Age vs Spending Score",
       x = "Age",
       y = "Spending Score")



############################################################
# 9. Optional: Save Segmentation Dataset for Next Week
############################################################

# Convert scaled data to a data frame for saving
segmentation_scaled_df <- as.data.frame(segmentation_scaled)

# Save if needed
# write.csv(segmentation_scaled_df,
#           "mall_customers_segmentation_scaled.csv",
#           row.names = FALSE)



############################################################
# 10. Final Notes for Students
############################################################

# Key takeaway:
# Week 4 focuses on understanding that customers differ
# in their characteristics and behaviors. These differences
# form the basis of market segmentation. In the next step,
# we will use clustering methods to identify customer groups
# more formally using data-driven techniques.



############################################################
# End of Week 4 Script
#
# Students should now be able to:
# - Explore behavioral differences in customer data
# - Recognize customer heterogeneity
# - Connect data analysis to the STP framework
# - Prepare variables for segmentation analysis
############################################################