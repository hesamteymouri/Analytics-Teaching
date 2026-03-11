############################################################
# RFM Analysis in R
# Recency – Frequency – Monetary Customer Segmentation
############################################################

# -----------------------------
# 1. Install and load packages
# -----------------------------
packages <- c("dplyr", "lubridate", "ggplot2", "readr")
installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

# -----------------------------
# 2. Example transaction data
# -----------------------------
# Replace this section with your own data if needed.
# Required columns:
# customer_id, order_date, order_value

transactions <- data.frame(
  customer_id = c(
    101,101,101,
    102,102,
    103,
    104,104,
    105,105,105,
    106,
    107,107,107,107,
    108,108,
    109,
    110,110,110
  ),
  order_date = c(
    "2024-01-10","2024-03-02","2024-04-01",
    "2024-02-11","2024-05-03",
    "2024-03-15",
    "2024-01-01","2024-04-22",
    "2024-02-05","2024-02-25","2024-04-15",
    "2024-03-20",
    "2024-01-12","2024-02-18","2024-03-25","2024-05-20",
    "2024-01-30","2024-02-28",
    "2024-04-12",
    "2024-01-05","2024-03-10","2024-05-01"
  ),
  order_value = c(
    120, 90, 150,
    45, 60,
    200,
    30, 75,
    80, 120, 200,
    50,
    40, 60, 70, 95,
    130, 150,
    300,
    25, 55, 65
  )
)

transactions$order_date <- as.Date(transactions$order_date)

# ---------------------------------------------
# 3. Optional: read data from a CSV file instead
# ---------------------------------------------
# Uncomment and edit the file path if you want to use your own data.
# The CSV should contain: customer_id, order_date, order_value
#
# transactions <- read_csv("transactions.csv")
# transactions$order_date <- as.Date(transactions$order_date)

# -----------------------------
# 4. Define analysis date
# -----------------------------
# Usually this is the date on which you run the analysis.
analysis_date <- as.Date("2024-06-01")

# -----------------------------
# 5. Calculate RFM metrics
# -----------------------------
rfm_table <- transactions %>%
  group_by(customer_id) %>%
  summarise(
    Recency = as.numeric(analysis_date - max(order_date)),
    Frequency = n(),
    Monetary = sum(order_value, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n====================\n")
cat("RFM TABLE\n")
cat("====================\n")
print(rfm_table)

# -----------------------------
# 6. Create RFM scores
# -----------------------------
# Note:
# Lower Recency is better, so we reverse the ranking for Recency.
rfm_scores <- rfm_table %>%
  mutate(
    R_score = ntile(desc(Recency), 5),   # lower recency = better score
    F_score = ntile(Frequency, 5),
    M_score = ntile(Monetary, 5),
    RFM_segment = paste0(R_score, F_score, M_score),
    RFM_total = R_score + F_score + M_score
  )

cat("\n====================\n")
cat("RFM SCORES\n")
cat("====================\n")
print(rfm_scores)

# -----------------------------
# 7. Create segment labels
# -----------------------------
rfm_scores <- rfm_scores %>%
  mutate(
    Segment = case_when(
      R_score >= 4 & F_score >= 4 & M_score >= 4 ~ "Champions",
      R_score >= 3 & F_score >= 4 & M_score >= 3 ~ "Loyal Customers",
      R_score >= 4 & F_score <= 2 ~ "New Customers",
      R_score <= 2 & F_score >= 3 & M_score >= 3 ~ "At Risk",
      R_score <= 2 & F_score <= 2 & M_score <= 2 ~ "Hibernating",
      TRUE ~ "Potential"
    )
  )

cat("\n====================\n")
cat("CUSTOMER SEGMENTS\n")
cat("====================\n")
print(rfm_scores)

# -----------------------------
# 8. Segment summary table
# -----------------------------
segment_summary <- rfm_scores %>%
  group_by(Segment) %>%
  summarise(
    Customers = n(),
    Avg_Recency = round(mean(Recency), 2),
    Avg_Frequency = round(mean(Frequency), 2),
    Avg_Monetary = round(mean(Monetary), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(Customers))

cat("\n====================\n")
cat("SEGMENT SUMMARY\n")
cat("====================\n")
print(segment_summary)

# -----------------------------
# 9. Top customers
# -----------------------------
top_customers <- rfm_scores %>%
  arrange(desc(Monetary), desc(Frequency), Recency) %>%
  slice_head(n = 10)

cat("\n====================\n")
cat("TOP CUSTOMERS\n")
cat("====================\n")
print(top_customers)

# -----------------------------
# 10. Visualization 1:
# Frequency vs Monetary
# -----------------------------
plot1 <- ggplot(rfm_scores, aes(x = Frequency, y = Monetary, color = Segment)) +
  geom_point(size = 4) +
  labs(
    title = "Customer Segmentation using RFM",
    x = "Frequency",
    y = "Monetary Value"
  ) +
  theme_minimal()

print(plot1)

# -----------------------------
# 11. Visualization 2:
# Segment counts
# -----------------------------
plot2 <- ggplot(segment_summary, aes(x = reorder(Segment, Customers), y = Customers)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Number of Customers by Segment",
    x = "Segment",
    y = "Number of Customers"
  ) +
  theme_minimal()

print(plot2)

# -----------------------------
# 12. Export results
# -----------------------------
write_csv(rfm_scores, "rfm_scores_output.csv")
write_csv(segment_summary, "rfm_segment_summary.csv")

cat("\nFiles exported:\n")
cat("- rfm_scores_output.csv\n")
cat("- rfm_segment_summary.csv\n")

############################################################
# End of script
############################################################
