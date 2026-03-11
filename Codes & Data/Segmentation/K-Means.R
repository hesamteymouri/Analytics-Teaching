############################################################
# K-Means Clustering in R
# Customer / Market Segmentation Example
############################################################

############################
# 1. Install and load packages
############################

# Install packages only if needed
# install.packages("tidyverse")
# install.packages("factoextra")

library(tidyverse)
library(factoextra)

############################
# 2. Set working directory
############################

# Change this path to the folder where your data file is stored
setwd("C:/Users/gduman/OneDrive - University of New Haven/Desktop/UNH/MKTG6640_4450/Fall2023/week6")

############################
# 3. Read the data
############################

# Read the CSV file
# header = TRUE means the first row contains variable names
seg_data <- read.csv("SegmentationData.csv", header = TRUE, sep = ",")

# Inspect the first few rows
head(seg_data)

# Check the structure of the data
str(seg_data)

############################
# 4. Select variables for clustering
############################

# We use columns 2 through 7 for clustering
# Usually column 1 contains an ID and should not be included
cluster_data <- seg_data[, 2:7]

############################
# 5. Standardize or normalize the data
############################

# Option A: Z-score standardization
# This makes each variable have mean 0 and standard deviation 1
std_seg_data <- scale(cluster_data)

# Option B: Min-max normalization
# This rescales each variable to a 0-1 range
func_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

norm_seg_data <- as.data.frame(lapply(cluster_data, func_normalize))

# View normalized data
View(norm_seg_data)

############################
# 6. Determine the number of clusters
############################

# Elbow method:
# This helps evaluate how many clusters may be appropriate
fviz_nbclust(norm_seg_data, kmeans, method = "wss", k.max = 15) +
  ggtitle("Elbow Method for Choosing Number of Clusters")

# Optional:
# You can also use silhouette method
fviz_nbclust(norm_seg_data, kmeans, method = "silhouette", k.max = 15) +
  ggtitle("Silhouette Method for Choosing Number of Clusters")

############################
# 7. Run K-means clustering
############################

# Set a seed so results are reproducible
set.seed(42)

# Run K-means with 3 clusters
# centers = number of clusters
# nstart = number of random initial configurations
# iter.max = maximum number of iterations
km <- kmeans(norm_seg_data, centers = 3, nstart = 25, iter.max = 100)

############################
# 8. Examine K-means results
############################

# Cluster membership for each observation
km$cluster

# Add cluster assignments to the original dataset
seg_data$cluster_km <- km$cluster

# Centroids (average profile of each cluster)
km$centers

# Within-cluster sum of squares
km$withinss

# Total within-cluster sum of squares
km$tot.withinss

# Between-cluster sum of squares
km$betweenss

# Cluster sizes
km$size

# View first few rows with cluster labels
head(seg_data)

############################
# 9. Summarize cluster profiles
############################

# Compute mean values for each variable by cluster
cluster_summary <- seg_data %>%
  group_by(cluster_km) %>%
  summarise(across(2:7, mean, na.rm = TRUE))

print(cluster_summary)

############################
# 10. Visualize K-means clusters
############################

# Basic cluster visualization
fviz_cluster(
  km,
  data = norm_seg_data,
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "K-Means Cluster Visualization"
)

# Improved cluster plot with convex hulls
fviz_cluster(
  km,
  data = norm_seg_data,
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal(),
  main = "K-Means Clusters with Cluster Boundaries"
)

############################
# 11. Visualize cluster sizes
############################

# Frequency table of cluster membership
table(seg_data$cluster_km)

# Bar chart of cluster sizes
seg_data %>%
  count(cluster_km) %>%
  ggplot(aes(x = factor(cluster_km), y = n)) +
  geom_col() +
  labs(
    title = "Cluster Sizes",
    x = "Cluster",
    y = "Number of Observations"
  ) +
  theme_minimal()

############################
# 12. Optional: Export results
############################

# Save clustered dataset
write.csv(seg_data, "SegmentationData_with_KMeans_Clusters.csv", row.names = FALSE)

# Save cluster summary
write.csv(cluster_summary, "KMeans_Cluster_Summary.csv", row.names = FALSE)

############################################################
# End of script
############################################################
