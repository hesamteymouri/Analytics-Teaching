############################################################
# Marketing Analytics
# Week 9: Choice Modeling & Marketing Experiments
#
# Purpose of this script:
# This script introduces:
# 1. Choice-based modeling using a real choice dataset
# 2. A/B testing and experimental design
# 3. Statistical testing for marketing experiments
#
# Dataset for choice modeling:
# TravelMode.csv
#
# In this dataset, each individual considers multiple travel
# options (car, air, train, bus), and the variable "choice"
# indicates whether that option was chosen.
############################################################



############################################################
# 1. Load Required Packages
############################################################

# Install packages if needed
# install.packages("tidyverse")
# install.packages("mlogit")

library(tidyverse)
library(mlogit)



############################################################
# 2. Import the Choice Dataset
############################################################

data <- read.csv("TravelMode.csv")

# Inspect the data
head(data)
str(data)
summary(data)



############################################################
# 3. Understand the Variables
############################################################

# Typical variables in this dataset:
# individual = decision-maker ID
# mode       = travel alternative (car, air, train, bus)
# choice     = whether the alternative was chosen
# wait       = waiting time
# vcost      = vehicle cost
# travel     = travel time
# gcost      = generalized cost
# income     = household income
# size       = party size

# Check levels
table(data$mode)
table(data$choice)



############################################################
# 4. Basic Data Preparation
############################################################

# Remove any index column if it exists
# Some CSV versions include an extra first column called X
if ("X" %in% names(data)) {
  data <- data %>% select(-X)
}

# Convert variables to appropriate types
data$individual <- as.factor(data$individual)
data$mode <- as.factor(data$mode)
data$choice <- as.factor(data$choice)

# Check missing values
colSums(is.na(data))



############################################################
# 5. Exploratory Analysis of Choice Behavior
############################################################

# How often was each travel mode chosen?
chosen_modes <- data %>%
  filter(choice == "yes") %>%
  count(mode)

chosen_modes

# Bar chart of chosen modes
ggplot(chosen_modes, aes(x = mode, y = n)) +
  geom_col() +
  labs(title = "Chosen Travel Modes",
       x = "Mode",
       y = "Number of Times Chosen")

# Compare travel time by mode
ggplot(data, aes(x = mode, y = travel)) +
  geom_boxplot() +
  labs(title = "Travel Time by Mode",
       x = "Travel Mode",
       y = "Travel Time")

# Compare vehicle cost by mode
ggplot(data, aes(x = mode, y = vcost)) +
  geom_boxplot() +
  labs(title = "Vehicle Cost by Mode",
       x = "Travel Mode",
       y = "Vehicle Cost")

# Compare waiting time by mode
ggplot(data, aes(x = mode, y = wait)) +
  geom_boxplot() +
  labs(title = "Waiting Time by Mode",
       x = "Travel Mode",
       y = "Waiting Time")



############################################################
# 6. Convert Data to Choice-Model Format
############################################################

# mlogit requires data in a special format.
# This dataset is already in long format:
# one row per alternative per individual.

choice_data <- mlogit.data(data,
                           choice = "choice",
                           shape = "long",
                           alt.var = "mode",
                           chid.var = "individual")



############################################################
# 7. Estimate a Choice Model
############################################################

# Model intuition:
# Consumers are less likely to choose options with
# higher cost and longer time.

choice_model <- mlogit(choice ~ vcost + travel + wait,
                       data = choice_data,
                       reflevel = "car")

summary(choice_model)



############################################################
# 8. Optional Extended Model
############################################################

# Add an interaction-like variable often used in this dataset:
# income effect for air travel

data$incair <- with(data, income * (mode == "air"))

choice_data2 <- mlogit.data(data,
                            choice = "choice",
                            shape = "long",
                            alt.var = "mode",
                            chid.var = "individual")

choice_model2 <- mlogit(choice ~ gcost + wait + incair,
                        data = choice_data2,
                        reflevel = "car")

summary(choice_model2)



############################################################
# 9. Interpreting the Choice Model
############################################################

# Interpretation guide:
# Negative coefficient on vcost:
#   Higher cost lowers the probability of choosing that mode
#
# Negative coefficient on travel:
#   Longer travel time lowers the probability of choosing that mode
#
# Negative coefficient on wait:
#   Longer waiting time lowers the probability of choosing that mode

# These are exactly the kinds of tradeoffs studied in
# choice-based conjoint and discrete choice analysis.



############################################################
# 10. A/B Testing and Experimental Design
############################################################

# Now we switch from choice modeling to marketing experiments.
# Here we simulate a simple A/B test for two ad versions.

set.seed(123)

n <- 1000

experiment <- data.frame(
  user_id = 1:n,
  group = sample(c("A", "B"), n, replace = TRUE)
)

# Simulate conversions:
# Group A = 8% conversion rate
# Group B = 12% conversion rate

experiment$conversion <- ifelse(
  experiment$group == "A",
  rbinom(n, 1, 0.08),
  rbinom(n, 1, 0.12)
)

head(experiment)



############################################################
# 11. Summarize the Experiment
############################################################

experiment %>%
  group_by(group) %>%
  summarize(
    users = n(),
    conversions = sum(conversion),
    conversion_rate = mean(conversion)
  )

# Visualize conversion rates
ggplot(experiment, aes(x = group, fill = factor(conversion))) +
  geom_bar(position = "fill") +
  labs(title = "Conversion Rates by Experimental Group",
       x = "Group",
       y = "Proportion",
       fill = "Converted")



############################################################
# 12. Statistical Testing for the A/B Test
############################################################

# Chi-square test
ab_table <- table(experiment$group, experiment$conversion)
ab_table

chisq.test(ab_table)

# Two-sample proportion test
prop.test(x = c(sum(experiment$conversion[experiment$group == "A"]),
                sum(experiment$conversion[experiment$group == "B"])),
          n = c(sum(experiment$group == "A"),
                sum(experiment$group == "B")))



############################################################
# 13. Experimental Interpretation
############################################################

# If the p-value is below 0.05:
# The difference in conversion rates is statistically significant.
#
# Marketing meaning:
# If Group B converts better than Group A,
# the firm may prefer to use Version B.



############################################################
# End of Week 9 Script
#
# Students should now be able to:
# - Understand what a true choice dataset looks like
# - Estimate a discrete choice model
# - Interpret attribute tradeoffs in consumer choice
# - Understand the basics of A/B testing
# - Conduct statistical tests for experiments
############################################################
