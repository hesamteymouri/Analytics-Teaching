############################################################
# Marketing Analytics
# Week 4: Exploratory Analysis for Customer Behavior & STP
#
# Purpose of this script:
# This script explores customer behavioral patterns and
# prepares data for segmentation analysis.
#
# Students will learn how to:
# 1. Explore behavioral customer data
# 2. Identify customer heterogeneity
# 3. Connect findings to the STP framework
# 4. Prepare variables for segmentation
############################################################



############################################################
# 1. Load Packages
############################################################

library(tidyverse)



############################################################
# 2. Import Dataset
############################################################

data <- read.csv("Mall_Customers.csv")

head(data)
str(data)



############################################################
# 3. Rename Variables for Easier Coding
############################################################

names(data) <- c("customer_id",
                 "gender",
                 "age",
                 "income",
                 "spending_score")

names(data)



############################################################
# 4. Basic Data Preparation
############################################################

# Convert gender to categorical variable
data$gender <- as.factor(data$gender)

# Check for missing values
colSums(is.na(data))

summary(data)



############################################################
# 5. Behavioral Data Exploration
############################################################

# Spending score is the main behavioral indicator

# Distribution of spending behavior
hist(data$spending_score,
     main = "Distribution of Customer Spending Score",
     xlab = "Spending Score",
     col = "lightblue")

# Compare spending by gender
ggplot(data, aes(x = gender, y = spending_score)) +
  geom_boxplot() +
  labs(title = "Customer Spending Score by Gender")

# Relationship between income and spending behavior
ggplot(data, aes(x = income, y = spending_score)) +
  geom_point(alpha = .7) +
  labs(title = "Income vs Spending Score",
       x = "Annual Income",
       y = "Spending Score")

# Relationship between age and spending behavior
ggplot(data, aes(x = age, y = spending_score)) +
  geom_point(alpha = .7) +
  labs(title = "Age vs Spending Score")



############################################################
# 6. Customer Heterogeneity
############################################################

# Customers are not identical. We explore how they differ.

# Create age groups
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
    avg_spending = mean(spending_score),
    avg_income = mean(income),
    customers = n()
  )

# Visualize spending differences
ggplot(data, aes(x = age_group, y = spending_score)) +
  geom_boxplot() +
  labs(title = "Spending Score by Age Group")



############################################################
# 7. STP Framework
############################################################

# STP = Segmentation, Targeting, Positioning

# Step 1: Identify potential segments in the data

# Create simple spending categories
data <- data %>%
  mutate(spending_group = case_when(
    spending_score < 34 ~ "Low Spending",
    spending_score >= 34 & spending_score < 67 ~ "Medium Spending",
    spending_score >= 67 ~ "High Spending"
  ))

data$spending_group <- as.factor(data$spending_group)

# Examine number of customers in each group
table(data$spending_group)

# Compare customer characteristics
data %>%
  group_by(spending_group) %>%
  summarize(
    avg_age = mean(age),
    avg_income = mean(income),
    customers = n()
  )

# Visualization
ggplot(data, aes(x = spending_group, y = income)) +
  geom_boxplot() +
  labs(title = "Income by Spending Group")



############################################################
# 8. Preparing Data for Segmentation
############################################################

# For segmentation we usually:
# 1. Remove ID variables
# 2. Select numeric variables
# 3. Scale the variables

segmentation_data <- data %>%
  select(age, income, spending_score)

head(segmentation_data)

summary(segmentation_data)



############################################################
# 9. Scale Variables
############################################################

scaled_data <- scale(segmentation_data)

head(scaled_data)

# Check scaling results
apply(scaled_data, 2, mean)
apply(scaled_data, 2, sd)



############################################################
# 10. Visualizing Variables for Segmentation
############################################################

# Scatterplots help reveal possible customer groups

pairs(segmentation_data,
      main = "Customer Segmentation Variables")

ggplot(data, aes(x = income, y = spending_score)) +
  geom_point() +
  labs(title = "Income vs Spending Score")



############################################################
# 11. Save Dataset for Next Week
############################################################

scaled_df <- as.data.frame(scaled_data)

# write.csv(scaled_df,
#           "mall_customers_scaled.csv",
#           row.names = FALSE)



############################################################
# End of Week 4 Script
#
# Students should now understand:
# - Behavioral differences among customers
# - Customer heterogeneity
# - How STP relates to data
# - How to prepare variables for segmentation
############################################################
