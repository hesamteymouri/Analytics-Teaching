############################################################
# Marketing Analytics
# Week 9: Choice Modeling & Marketing Experiments
#
# Purpose of this script:
# Students will learn how to:
# 1. Understand choice modeling concepts
# 2. Run a simple conjoint-style analysis
# 3. Conduct A/B testing
# 4. Perform statistical tests for experiments
############################################################



############################################################
# 1. Load Required Packages
############################################################

library(tidyverse)



############################################################
# 2. Import Product Choice Dataset
############################################################

data <- read.csv("mobile_phone_data.csv")

head(data)
str(data)



############################################################
# 3. Understanding Product Attributes
############################################################

# In choice modeling, consumers choose between products
# based on product attributes.

summary(data)



############################################################
# 4. Exploratory Analysis of Product Features
############################################################

# Distribution of price
hist(data$price_range,
     main="Distribution of Phone Price Category",
     xlab="Price Category",
     col="lightblue")

# Compare RAM across price categories
ggplot(data, aes(x=factor(price_range), y=ram)) +
  geom_boxplot() +
  labs(title="RAM by Price Category",
       x="Price Category",
       y="RAM")



############################################################
# 5. Simple Choice Modeling Example
############################################################

# Predict whether a phone belongs to a high price category
# based on its attributes

choice_model <- glm(price_range ~ ram + battery_power + px_height + px_width,
                    data=data,
                    family=binomial)

summary(choice_model)



############################################################
# 6. Simulating an A/B Test
############################################################

# Example: testing two marketing messages

set.seed(123)

n <- 1000

experiment <- data.frame(
  group = sample(c("A","B"), n, replace=TRUE)
)

# Simulate conversion behavior
experiment$conversion <- ifelse(
  experiment$group=="A",
  rbinom(n,1,0.08),   # 8% conversion
  rbinom(n,1,0.12)    # 12% conversion
)

head(experiment)



############################################################
# 7. Conversion Rates
############################################################

experiment %>%
  group_by(group) %>%
  summarize(
    conversion_rate = mean(conversion),
    users = n()
  )



############################################################
# 8. Visualizing the Experiment
############################################################

ggplot(experiment, aes(x=group, fill=factor(conversion))) +
  geom_bar(position="fill") +
  labs(title="Conversion Rate by Experiment Group",
       y="Proportion")



############################################################
# 9. Statistical Testing
############################################################

# Test if conversion rates differ

table_test <- table(experiment$group,
                    experiment$conversion)

chisq.test(table_test)



############################################################
# 10. Interpreting Results
############################################################

# If the p-value is below 0.05,
# the difference between groups is statistically significant.

# Marketing interpretation:
# If Group B has a higher conversion rate,
# the company should adopt message B.



############################################################
# End of Week 9 Script
############################################################
