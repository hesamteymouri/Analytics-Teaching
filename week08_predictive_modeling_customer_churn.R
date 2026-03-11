############################################################
# Marketing Analytics
# Week 8: Predictive Modeling & Customer Churn
#
# Purpose of this script:
# This script introduces predictive analytics using a
# customer churn dataset. Students will learn how to:
#
# 1. Understand the idea of predictive modeling
# 2. Build a logistic regression model
# 3. Predict customer churn
# 4. Interpret model results
############################################################



############################################################
# 1. Load Required Packages
############################################################

# Install packages if needed
# install.packages("tidyverse")
# install.packages("caret")

library(tidyverse)
library(caret)



############################################################
# 2. Import Dataset
############################################################

# Load the Telco Customer Churn dataset
data <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",
                 stringsAsFactors = FALSE)

# View first rows
head(data)

# Inspect structure
str(data)

# View variable names
names(data)



############################################################
# 3. Basic Data Preparation
############################################################

# Check missing values
colSums(is.na(data))

# In this dataset, TotalCharges is sometimes stored as text
# and may contain blank spaces. We convert it to numeric.

data$TotalCharges[data$TotalCharges == ""] <- NA
data$TotalCharges <- as.numeric(data$TotalCharges)

# Remove rows with missing values
data <- na.omit(data)

# Check structure again
str(data)



############################################################
# 4. Convert Variables to Appropriate Types
############################################################

# Convert target variable to factor
data$Churn <- as.factor(data$Churn)

# Convert selected categorical variables to factors
data$gender <- as.factor(data$gender)
data$Partner <- as.factor(data$Partner)
data$Dependents <- as.factor(data$Dependents)
data$PhoneService <- as.factor(data$PhoneService)
data$MultipleLines <- as.factor(data$MultipleLines)
data$InternetService <- as.factor(data$InternetService)
data$OnlineSecurity <- as.factor(data$OnlineSecurity)
data$OnlineBackup <- as.factor(data$OnlineBackup)
data$DeviceProtection <- as.factor(data$DeviceProtection)
data$TechSupport <- as.factor(data$TechSupport)
data$StreamingTV <- as.factor(data$StreamingTV)
data$StreamingMovies <- as.factor(data$StreamingMovies)
data$Contract <- as.factor(data$Contract)
data$PaperlessBilling <- as.factor(data$PaperlessBilling)
data$PaymentMethod <- as.factor(data$PaymentMethod)

# SeniorCitizen is coded as 0/1 in the dataset.
# We convert it to a factor for easier interpretation.
data$SeniorCitizen <- as.factor(data$SeniorCitizen)



############################################################
# 5. Exploratory View of Churn
############################################################

# Frequency of churn
table(data$Churn)

# Proportion of churn
prop.table(table(data$Churn))

# Churn by contract type
table(data$Contract, data$Churn)

# Churn by internet service
table(data$InternetService, data$Churn)

# Visualization: churn count
ggplot(data, aes(x = Churn)) +
  geom_bar() +
  labs(title = "Customer Churn Count",
       x = "Churn",
       y = "Count")

# Visualization: MonthlyCharges by churn
ggplot(data, aes(x = Churn, y = MonthlyCharges)) +
  geom_boxplot() +
  labs(title = "Monthly Charges by Churn Status",
       x = "Churn",
       y = "Monthly Charges")

# Visualization: tenure by churn
ggplot(data, aes(x = Churn, y = tenure)) +
  geom_boxplot() +
  labs(title = "Tenure by Churn Status",
       x = "Churn",
       y = "Tenure")



############################################################
# 6. Select Variables for Predictive Modeling
############################################################

# Remove customerID because it is only an identifier
model_data <- data %>%
  select(-customerID)

# Check structure
str(model_data)



############################################################
# 7. Split Data into Training and Test Sets
############################################################

set.seed(123)

train_index <- createDataPartition(model_data$Churn,
                                   p = 0.70,
                                   list = FALSE)

train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]



############################################################
# 8. Build Logistic Regression Model
############################################################

# Logistic regression is used when the outcome variable
# is binary (e.g., churn vs no churn)

churn_model <- glm(Churn ~ tenure +
                     MonthlyCharges +
                     TotalCharges +
                     Contract +
                     InternetService +
                     OnlineSecurity +
                     TechSupport +
                     PaperlessBilling +
                     PaymentMethod +
                     SeniorCitizen,
                   data = train_data,
                   family = binomial)

# View model summary
summary(churn_model)



############################################################
# 9. Interpret Logistic Regression Output
############################################################

# Convert coefficients to odds ratios for easier interpretation
exp(coef(churn_model))

# Example interpretation:
# - A positive coefficient increases the likelihood of churn
# - A negative coefficient decreases the likelihood of churn
# - Odds ratios above 1 suggest higher odds of churn
# - Odds ratios below 1 suggest lower odds of churn



############################################################
# 10. Predict Churn Probabilities
############################################################

# Predict churn probabilities for test data
pred_prob <- predict(churn_model,
                     newdata = test_data,
                     type = "response")

# View first few predicted probabilities
head(pred_prob)

# Convert probabilities to Yes/No predictions
pred_class <- ifelse(pred_prob > 0.50, "Yes", "No")

pred_class <- as.factor(pred_class)

# Make sure factor levels match actual data
pred_class <- factor(pred_class, levels = levels(test_data$Churn))



############################################################
# 11. Evaluate Model Performance
############################################################

# Confusion matrix
confusionMatrix(pred_class, test_data$Churn)

# This gives:
# - accuracy
# - sensitivity
# - specificity
# - other classification metrics



############################################################
# 12. Model Interpretation for Marketing
############################################################

# Compare average predicted churn probability by contract type
test_results <- test_data %>%
  mutate(predicted_probability = pred_prob)

test_results %>%
  group_by(Contract) %>%
  summarize(avg_predicted_churn = mean(predicted_probability),
            customer_count = n())

# Compare average predicted churn probability by internet service
test_results %>%
  group_by(InternetService) %>%
  summarize(avg_predicted_churn = mean(predicted_probability),
            customer_count = n())

# Compare average predicted churn probability by payment method
test_results %>%
  group_by(PaymentMethod) %>%
  summarize(avg_predicted_churn = mean(predicted_probability),
            customer_count = n())



############################################################
# 13. Optional Visualization of Predicted Probabilities
############################################################

ggplot(test_results, aes(x = predicted_probability)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Predicted Churn Probabilities",
       x = "Predicted Probability of Churn",
       y = "Count")



############################################################
# End of Week 8 Script
#
# Students should now be able to:
# - Understand the idea of predictive analytics
# - Build a logistic regression model
# - Predict customer churn
# - Interpret model results in a marketing context
############################################################