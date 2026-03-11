############################################################
# Marketing Analytics
# Week 14: Association Rules & Market Basket Analysis
#
# Purpose of this script:
# This script introduces market basket analysis using
# association rules.
#
# Students will learn how to:
# 1. Prepare transactional retail data
# 2. Understand support, confidence, and lift
# 3. Apply the Apriori algorithm
# 4. Interpret retail product associations
############################################################



############################################################
# 1. Load Required Packages
############################################################

# install.packages("arules")
# install.packages("arulesViz")
# install.packages("tidyverse")

library(arules)
library(arulesViz)
library(tidyverse)



############################################################
# 2. Import the Retail Dataset
############################################################

data <- read.csv("OnlineRetail.csv", stringsAsFactors = FALSE)

head(data)
str(data)



############################################################
# 3. Data Cleaning
############################################################

# Remove rows without customer IDs
data <- data %>%
  filter(!is.na(CustomerID))

# Remove cancelled transactions
data <- data %>%
  filter(substr(InvoiceNo,1,1) != "C")

# Remove negative or zero quantities
data <- data %>%
  filter(Quantity > 0)

# Convert invoice number to character
data$InvoiceNo <- as.character(data$InvoiceNo)



############################################################
# 4. Prepare Transaction Data
############################################################

# Each invoice represents a basket of products

basket_data <- data %>%
  select(InvoiceNo, Description)

# Remove empty descriptions
basket_data <- basket_data %>%
  filter(Description != "")



############################################################
# 5. Convert Data to Transaction Format
############################################################

transactions_list <- split(basket_data$Description,
                           basket_data$InvoiceNo)

transactions <- as(transactions_list, "transactions")

transactions



############################################################
# 6. Inspect Transaction Data
############################################################

summary(transactions)

# Top purchased items
itemFrequencyPlot(transactions,
                  topN = 20,
                  type = "absolute",
                  main = "Top 20 Products")



############################################################
# 7. Understanding Support, Confidence, Lift
############################################################

# Support
# Proportion of baskets containing an itemset

# Confidence
# Probability that item B is purchased when item A is purchased

# Lift
# How much more likely items occur together than expected by chance



############################################################
# 8. Run the Apriori Algorithm
############################################################

rules <- apriori(transactions,
                 parameter = list(
                   support = 0.01,
                   confidence = 0.3,
                   minlen = 2
                 ))

summary(rules)



############################################################
# 9. Inspect Top Rules
############################################################

# Sort rules by lift
top_rules <- sort(rules, by = "lift", decreasing = TRUE)

inspect(top_rules[1:10])



############################################################
# 10. Visualize Association Rules
############################################################

plot(top_rules[1:20],
     method = "graph",
     engine = "htmlwidget")



############################################################
# 11. Retail Analytics Interpretation
############################################################

# Example interpretation:

# If a rule shows:
# {Bread} -> {Butter}

# Support = 0.05
# 5% of all baskets contain both bread and butter

# Confidence = 0.60
# 60% of customers who buy bread also buy butter

# Lift = 2.0
# Customers buying bread are twice as likely to buy butter
# compared to random chance



############################################################
# 12. Example Managerial Questions
############################################################

# - Which products are frequently purchased together?
# - Which items should be placed together in stores?
# - Which bundles could increase cross-selling?
# - Which items should appear in recommendation systems?



############################################################
# End of Week 14 Script
############################################################
