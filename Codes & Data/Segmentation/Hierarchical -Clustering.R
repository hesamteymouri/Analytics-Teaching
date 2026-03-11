############################################################
# Hierarchical Clustering
############################################################

# Step 1: Compute distance matrix
# Euclidean distance is commonly used in clustering
dist_matrix <- dist(std_seg_data2)

############################################################
# Step 2: Perform hierarchical clustering
############################################################

# Ward's method minimizes within-cluster variance
hc <- hclust(dist_matrix, method = "ward.D2")

############################################################
# Step 3: Plot the dendrogram
############################################################

# Dendrogram shows how observations merge into clusters

plot(hc,
     main = "Hierarchical Clustering Dendrogram",
     xlab = "Customers",
     ylab = "Distance",
     cex = 0.6)

############################################################
# Step 4: Highlight clusters on the dendrogram
############################################################

# rect.hclust draws rectangles around clusters
rect.hclust(hc, k = 3, border = "red")

############################################################
# Step 5: Assign cluster membership
############################################################

clusters_hc <- cutree(hc, k = 3)

# Add cluster labels to original dataset
seg_data$cluster_hc <- clusters_hc

############################################################
# Step 6: Check cluster sizes
############################################################

table(seg_data$cluster_hc)

############################################################
# Step 7: Visualize clusters using factoextra
############################################################

# install.packages("factoextra")
library(factoextra)

fviz_cluster(
  list(data = std_seg_data2, cluster = clusters_hc),
  geom = "point",
  palette = "rgb",
  main = "Hierarchical Clustering Visualization"
)
