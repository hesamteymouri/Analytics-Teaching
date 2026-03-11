############################################################
# Marketing Analytics
# Week 1: Introduction to the R Environment
#
# Purpose of this script:
# This script introduces the R programming environment and 
# basic commands used for data analysis. Students will learn:
#
# 1. How to use R and RStudio
# 2. How to install and load packages
# 3. How to import a dataset
# 4. How to explore and inspect data
# 5. Basic descriptive operations
#
# NOTE:
# Run this script line by line in RStudio to see how each 
# command works.
############################################################


############################################################
# 1. Install Packages
############################################################

# Packages extend the functionality of R.
# The tidyverse is a collection of tools for data manipulation
# and visualization.

# Install the package (only needed once)
install.packages("tidyverse")

# Load the package into the current session
library(tidyverse)



############################################################
# 2. Understanding the R Environment
############################################################

# R works like a calculator

2 + 2

10 * 5

100 / 4

# R can also store values using variables

revenue <- 12000
cost <- 8000

profit <- revenue - cost

profit



############################################################
# 3. Working with Vectors
############################################################

# A vector is a list of values

sales <- c(120, 150, 200, 175, 220)

sales

# Basic calculations

mean(sales)
sum(sales)
max(sales)
min(sales)



############################################################
# 4. Importing Data
############################################################

# In marketing analytics we usually analyze datasets.

# Example: load a CSV dataset

customer_data <- read.csv("customer_data.csv")

# View first rows of the dataset
head(customer_data)

# View dataset structure
str(customer_data)

# View column names
names(customer_data)



############################################################
# 5. Inspecting the Dataset
############################################################

# Number of rows and columns

dim(customer_data)

# Summary statistics

summary(customer_data)



############################################################
# 6. Selecting Columns
############################################################

# Example: select a variable

customer_data$age

# Average age

mean(customer_data$age)



############################################################
# 7. Simple Data Visualization
############################################################

# Histogram of customer age

hist(customer_data$age,
     main = "Distribution of Customer Age",
     xlab = "Age",
     col = "lightblue")



############################################################
# 8. Using the Tidyverse
############################################################

# The tidyverse provides tools for modern data analysis

# Example: calculate average spending

customer_data %>%
  summarize(avg_spending = mean(spending, na.rm = TRUE))



############################################################
# 9. Creating a Simple Plot with ggplot
############################################################

ggplot(customer_data, aes(x = age, y = spending)) +
  geom_point() +
  labs(title = "Customer Age vs Spending",
       x = "Age",
       y = "Spending")



############################################################
# End of Week 1 Script
#
# Students should now be able to:
# - Run commands in R
# - Load packages
# - Import datasets
# - Explore basic dataset structure
# - Produce simple visualizations
############################################################
