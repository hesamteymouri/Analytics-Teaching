############################################################
# Week 6: Customer Segmentation & Clustering Analysis II
#
# Topics:
# 1. Principal Component Analysis (PCA)
# 2. Dimensionality Reduction
# 3. K-means Clustering
# 4. Segment Validation
############################################################

############################
# 1. Install and load packages
############################

# Install if needed
# install.packages("tidyverse")
# install.packages("factoextra")
# install.packages("cluster")

library(tidyverse)
library(factoextra)
library(cluster)

############################
# 2. Load dataset
############################

mall_data <- read.csv("Mall_Customers.csv")

# Inspect the dataset
head(mall_data)
str(mall_data)

############################
# 3. Select clustering variables
############################

# Remove CustomerID and Gender for PCA
cluster_data <- mall_data[, c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]

############################
# 4. Standardize variables
############################

# PCA requires standardized variables
cluster_scaled <- scale(cluster_data)

############################
# 5. Principal Component Analysis (PCA)
############################

pca_model <- prcomp(cluster_scaled)

# View PCA summary
summary(pca_model)

# Examine loadings (how variables contribute to components)
pca_model$rotation

############################
# 6. PCA Visualization
############################

# Scree plot
fviz_eig(pca_model,
         addlabels = TRUE,
         main = "Scree Plot: Variance Explained by Components")

# PCA variable contribution plot
fviz_pca_var(pca_model,
             col.var = "contrib",
             gradient.cols = c("blue", "yellow", "red"),
             repel = TRUE)

# PCA observation plot
fviz_pca_ind(pca_model,
             geom = "point",
             col.ind = "steelblue",
             repel = TRUE)

############################
# 7. Dimensionality Reduction
############################

# Extract first two principal components
pca_data <- as.data.frame(pca_model$x[,1:2])

# View reduced data
head(pca_data)

############################
# 8. Determine number of clusters
############################

# Elbow method
fviz_nbclust(cluster_scaled, kmeans,
             method = "wss",
             k.max = 10) +
  ggtitle("Elbow Method for Choosing Number of Clusters")

# Silhouette method
fviz_nbclust(cluster_scaled, kmeans,
             method = "silhouette",
             k.max = 10) +
  ggtitle("Silhouette Method")

############################
# 9. Run K-means clustering
############################

set.seed(123)

km <- kmeans(cluster_scaled,
             centers = 5,
             nstart = 25)

############################
# 10. Examine cluster results
############################

# Cluster assignments
km$cluster

# Add clusters to dataset
mall_data$cluster_kmeans <- as.factor(km$cluster)

# Cluster sizes
table(mall_data$cluster_kmeans)

# Cluster centroids
km$centers

############################
# 11. Visualize clusters
############################

fviz_cluster(km,
             data = cluster_scaled,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "K-means Clustering Results")

############################
# 12. Segment validation
############################

# Silhouette analysis
sil <- silhouette(km$cluster, dist(cluster_scaled))

fviz_silhouette(sil)

# Average silhouette width
mean(sil[,3])

############################
# 13. Segment interpretation
############################

segment_summary <- mall_data %>%
  group_by(cluster_kmeans) %>%
  summarise(
    avg_age = mean(Age),
    avg_income = mean(Annual.Income..k..),
    avg_spending = mean(Spending.Score..1.100.),
    n_customers = n(),
    .groups = "drop"
  )

print(segment_summary)

############################
# 14. Visualize cluster profiles
############################

segment_long <- segment_summary %>%
  pivot_longer(
    cols = c(avg_age, avg_income, avg_spending),
    names_to = "variable",
    values_to = "value"
  )

ggplot(segment_long,
       aes(x = variable,
           y = value,
           group = cluster_kmeans,
           color = cluster_kmeans)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Cluster Profiles",
    x = "Variable",
    y = "Average Value",
    color = "Cluster"
  ) +
  theme_minimal()

############################
# 15. Marketing interpretation example
############################

# Example segment descriptions:
#
# Cluster 1: High income, high spending
# → "Premium customers"
#
# Cluster 2: High income, low spending
# → "Affluent but disengaged customers"
#
# Cluster 3: Young moderate spenders
# → "Emerging customers"
#
# Cluster 4: Low income low spending
# → "Low value customers"
#
# Cluster 5: Moderate income high engagement
# → "Core retail segment"

############################################################
# End of Week 6 Script
############################################################
