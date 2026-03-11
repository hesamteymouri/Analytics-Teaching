############################################################
# Week 5: Customer Segmentation & Clustering Analysis I
# Dataset: Mall Customers
#
# Topics covered:
# 1. Distance metrics
# 2. Hierarchical clustering
# 3. Dendrogram interpretation
# 4. Segment interpretation
############################################################

############################
# 1. Install and load packages
############################

# Install once if needed:
# install.packages("tidyverse")
# install.packages("factoextra")

library(tidyverse)
library(factoextra)

############################
# 2. Read the dataset
############################

# Make sure the CSV file is in your working directory
# Example file name: Mall_Customers.csv

mall_data <- read.csv("Mall_Customers.csv", header = TRUE)

# View the first few rows
head(mall_data)

# Check the structure of the data
str(mall_data)

############################
# 3. Select variables for clustering
############################

# We exclude:
# - CustomerID because it is just an identifier
# - Gender because this introductory clustering example focuses
#   on numeric variables and distance-based methods

cluster_data <- mall_data[, c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]

# Inspect the selected variables
head(cluster_data)

############################
# 4. Standardize the variables
############################

# Standardization is important because clustering is based on distance.
# If one variable has a much larger scale than another, it can dominate
# the clustering results.

cluster_data_scaled <- scale(cluster_data)

# View the standardized data
head(cluster_data_scaled)

############################
# 5. Distance metrics
############################

# Distance metrics tell us how similar or different customers are.

# Euclidean distance:
# Straight-line distance between observations
dist_euclidean <- dist(cluster_data_scaled, method = "euclidean")

# Manhattan distance:
# Sum of absolute differences across variables
dist_manhattan <- dist(cluster_data_scaled, method = "manhattan")

# View first part of the distance matrix
as.matrix(dist_euclidean)[1:5, 1:5]
as.matrix(dist_manhattan)[1:5, 1:5]

############################
# 6. Hierarchical clustering
############################

# Ward's method is commonly used in segmentation because it tends
# to create compact and interpretable clusters.

hc_ward <- hclust(dist_euclidean, method = "ward.D2")

# Optional comparison: complete linkage
hc_complete <- hclust(dist_euclidean, method = "complete")

############################
# 7. Dendrogram visualization
############################

# Plot dendrogram for Ward's method
plot(
  hc_ward,
  main = "Dendrogram: Hierarchical Clustering (Ward's Method)",
  xlab = "Customers",
  ylab = "Height / Distance",
  cex = 0.6
)

# Add rectangles to highlight a 5-cluster solution
# The Mall Customers dataset often shows useful segmentation
# around 4-6 clusters, so 5 is a reasonable starting point.
rect.hclust(hc_ward, k = 5, border = "red")

# Optional: compare with complete linkage
plot(
  hc_complete,
  main = "Dendrogram: Hierarchical Clustering (Complete Linkage)",
  xlab = "Customers",
  ylab = "Height / Distance",
  cex = 0.6
)

############################
# 8. Cut dendrogram into clusters
############################

# Assign each customer to one of 5 clusters
clusters_hc <- cutree(hc_ward, k = 5)

# Add cluster labels back to the original dataset
mall_data$cluster_hc <- as.factor(clusters_hc)

# View first few rows with cluster labels
head(mall_data)

# Check number of customers in each cluster
table(mall_data$cluster_hc)

############################
# 9. Visualize hierarchical clustering results
############################

# This function projects the clusters into two dimensions for visualization
fviz_cluster(
  list(data = cluster_data_scaled, cluster = clusters_hc),
  geom = "point",
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "Hierarchical Clustering Results"
)

############################
# 10. Segment interpretation
############################

# Summarize the average profile of each cluster
segment_summary <- mall_data %>%
  group_by(cluster_hc) %>%
  summarise(
    avg_age = mean(Age),
    avg_income = mean(Annual.Income..k..),
    avg_spending_score = mean(Spending.Score..1.100.),
    n_customers = n(),
    .groups = "drop"
  )

print(segment_summary)

############################
# 11. Visualize cluster profiles
############################

# Reshape the summary table for plotting
segment_summary_long <- segment_summary %>%
  pivot_longer(
    cols = c(avg_age, avg_income, avg_spending_score),
    names_to = "variable",
    values_to = "value"
  )

ggplot(segment_summary_long,
       aes(x = variable, y = value, group = cluster_hc, color = cluster_hc)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Cluster Profiles for Segment Interpretation",
    x = "Variable",
    y = "Average Value",
    color = "Cluster"
  ) +
  theme_minimal()

############################
# 12. Optional 2D marketing interpretation plot
############################

# This plot is especially useful for this dataset because
# Annual Income and Spending Score often reveal clear segments.

ggplot(mall_data,
       aes(x = Annual.Income..k..,
           y = Spending.Score..1.100.,
           color = cluster_hc)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Customer Segments by Income and Spending Score",
    x = "Annual Income (k$)",
    y = "Spending Score",
    color = "Cluster"
  ) +
  theme_minimal()

############################
# 13. Example interpretation notes
############################

# After reviewing the segment summary and plots, students can assign
# descriptive labels to the clusters. For example:
#
# - High income + high spending:
#   "High-value premium customers"
#
# - High income + low spending:
#   "Affluent but low-engagement customers"
#
# - Low income + high spending:
#   "Young enthusiastic shoppers"
#
# - Moderate income + moderate spending:
#   "Mainstream customers"
#
# - Older customers + lower spending:
#   "Conservative shoppers"

############################
# 14. Optional export
############################

# Save dataset with cluster labels
write.csv(mall_data, "Mall_Customers_with_HC_Clusters.csv", row.names = FALSE)

# Save segment summary
write.csv(segment_summary, "Mall_Customers_HC_Segment_Summary.csv", row.names = FALSE)

############################################################
# End of script
############################################################
