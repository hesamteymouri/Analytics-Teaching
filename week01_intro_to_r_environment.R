library(tidyverse)
setwd("C:/Users/gduman/OneDrive - University of New Haven/Desktop/UNH/MKTG6640_4450/Fall2023/week6")

seg_data<-read.csv("SegmentationData.csv", header=TRUE, sep=",")

std_seg_data<-scale(seg_data[, 2:7])

#install.packages("Nbclust")
library(NbClust)

install.packages("NbClust")
library(NbClust)
?NbClust()

std_seg_data<-scale(seg_data[,2:7])
?scale()

NbClust(data=std_seg_data, min.nc = 2, max.nc = 15, method = "ward.D2")

cor(std_seg_data)

#NbClust(data=abs(std_seg_data), min.nc = 2, max.nc = 15, method = "ward.D2")


#create our own normalization function
func_normalize<-function(x){return((x-min(x))/(max(x)-min(x)))}

std_seg_data2<-as.data.frame(lapply(seg_data[,2:7], func_normalize))
view(std_seg_data2)

NbClust(data=std_seg_data2, min.nc = 2, max.nc = 15, method = "ward.D2")


####K-Means Clustering####
seg_data<-read.csv("SegmentationData.csv", header=TRUE, sep=",")
set.seed(42)
?kmeans()

km<-kmeans(std_seg_data2, 3, iter.max=100)

#cluster membership
km$cluster

#centroids
km$centers

#within-cluster sum squares
km$withinss

#cluster size
km$size


#install.packages("factoextra")
library(factoextra)

#screeplot
fviz_cluster(km, data=std_seg_data2, palette="rgb")
?fviz_nbclust()
fviz_nbclust(std_seg_data2, kmeans, method="wss", k.max=20)



