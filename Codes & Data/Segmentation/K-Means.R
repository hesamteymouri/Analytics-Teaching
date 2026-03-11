############################################################
# Customer Segmentation with Normalization, NbClust, and K-Means
# This script:
# 1. Reads customer segmentation data
# 2. Standardizes / normalizes the variables
# 3. Uses NbClust to help choose the number of clusters
# 4. Runs K-means clustering
# 5. Examines cluster output
# 6. Visualizes clusters and the elbow plot
############################################################

# Load the tidyverse package for data handling
library(tidyverse)

# Set the working directory to the folder where your data file is stored
# Change this path to the location on your own computer
setwd("C:/Users/....")

# Read the segmentation dataset
# header = TRUE means the first row contains variable names
# sep = "," tells R the file is comma-separated
seg_data <- read.csv("SegmentationData.csv", header = TRUE, sep = ",")

# Inspect the first few rows of the dataset
head(seg_data)

# Check the structure of the dataset
# This helps confirm which columns are numeric and usable for clustering
str(seg_data)

############################################################
# Step 1: Standardize the clustering variables
############################################################

# We standardize columns 2 through 7 so that all variables are on the same scale
# scale() converts each variable to z-scores:
# mean = 0 and standard deviation = 1
# This is useful when variables are measured in different units
std_seg_data <- scale(seg_data[, 2:7])

# If needed, you can read the help file for scale()
?scale

############################################################
# Step 2: Use NbClust to explore the appropriate number of clusters
############################################################

# Install NbClust once if it is not already installed
# install.packages("NbClust")

# Load the package
library(NbClust)

# If needed, open the help page
?NbClust

# NbClust compares different clustering solutions and suggests
# how many clusters may be appropriate.
# min.nc = minimum number of clusters to test
# max.nc = maximum number of clusters to test
# method = "ward.D2" uses Ward's hierarchical clustering method
NbClust(data = std_seg_data, min.nc = 2, max.nc = 15, method = "ward.D2")

# Examine correlations among the standardized variables
# Highly correlated variables may influence clustering results
cor(std_seg_data)

# Optional alternative:
# Using absolute values can sometimes be explored, but this is not standard practice
# NbClust(data = abs(std_seg_data), min.nc = 2, max.nc = 15, method = "ward.D2")

############################################################
# Step 3: Create a min-max normalized version of the data
############################################################

# This custom function rescales each variable to a 0-1 range
# Formula:
# (x - minimum) / (maximum - minimum)
# This is another common approach when preparing data for clustering
func_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply the normalization function to columns 2 through 7
std_seg_data2 <- as.data.frame(lapply(seg_data[, 2:7], func_normalize))

# View the normalized data
View(std_seg_data2)

# Run NbClust again using the normalized data
# This lets us compare whether the recommended number of clusters
# is similar across preprocessing methods
NbClust(data = std_seg_data2, min.nc = 2, max.nc = 15, method = "ward.D2")

############################################################
# Step 4: K-Means Clustering
############################################################

# Re-read the original dataset if needed
# (Not strictly necessary here, since seg_data is already loaded,
# but included in case the script is run in sections)
seg_data <- read.csv("SegmentationData.csv", header = TRUE, sep = ",")

# Set a seed so results are reproducible
# K-means starts with random initial cluster centers,
# so setting a seed helps ensure the same results each time
set.seed(42)

# Open help for kmeans if needed
?kmeans

# Run K-means clustering with 3 clusters
# std_seg_data2 is the normalized dataset
# 3 = chosen number of clusters
# iter.max = maximum number of iterations allowed
km <- kmeans(std_seg_data2, 3, iter.max = 100)

############################################################
# Step 5: Examine K-means output
############################################################

# Cluster membership:
# Shows which cluster each observation belongs to
km$cluster

# Cluster centroids:
# Shows the average profile of each cluster across the variables
km$centers

# Within-cluster sum of squares:
# Measures how tightly grouped the observations are within each cluster
# Lower values generally indicate more compact clusters
km$withinss

# Cluster size:
# Number of observations in each cluster
km$size

############################################################
# Step 6: Visualize the cluster solution
############################################################

# Install factoextra once if needed
# install.packages("factoextra")

# Load factoextra for cluster visualization
library(factoextra)

# Visualize the K-means clusters
# This plot helps show how distinct the clusters are
# palette = "rgb" controls the color scheme
fviz_cluster(km, data = std_seg_data2, palette = "rgb")

# Open help for fviz_nbclust if needed
?fviz_nbclust

# Create an elbow plot using the within-cluster sum of squares (WSS)
# This helps evaluate how many clusters may be appropriate
# k.max = maximum number of clusters to test
fviz_nbclust(std_seg_data2, kmeans, method = "wss", k.max = 20)

############################################################
# End of script
############################################################
