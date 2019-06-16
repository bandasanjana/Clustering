#Author: Sanjana Banda
#Title: Assignment-4
#Purpose: Cluster Analysis

library(dplyr)
library(tidyverse)

# 1.
#******************************************************************************************************************************#
# (a) Import the data to R and remove the column(s) that you are not going to use. Copy the R code used below. 
dungaree.df <- read.csv("dungaree.csv")
dungaree.df <- select(dungaree.df,-c(1,6))#Removing the StoreID, and SalesSlot
#******************************************************************************************************************************#

#******************************************************************************************************************************#
# (b) Examine the input variables: Are there any unusual data values? Are there missing values that
# should be replaced?
data.frame(miss.val=sapply(dungaree.df, function(x)
  (sum(length(which(is.na(x))))))) 
summary(dungaree.df)
boxplot(dungaree.df$FASHION)
boxplot(dungaree.df$LEISURE)
boxplot(dungaree.df$STRETCH)
boxplot(dungaree.df$ORIGINAL)
#******************************************************************************************************************************#

#******************************************************************************************************************************#
# (c) Normalize the data. Copy the R code used below. What would happen if you did not standardize/normalize your inputs?
dungaree.df.norm <- sapply(dungaree.df, scale)
#******************************************************************************************************************************#


#******************************************************************************************************************************#
# (d) Run k-means clustering using a seed = 42, and choose k = 20. Copy the R code used below.
# run kmeans algorithm 
set.seed(42)
km <- kmeans(dungaree.df.norm, 20)
#******************************************************************************************************************************#


#******************************************************************************************************************************#
# (f) In the next run, specify a maximum of six clusters, and run the k-means clustering algorithm again. Copy the R code used below.
 
km2 <- kmeans(dungaree.df.norm, 6)

#******************************************************************************************************************************#


#******************************************************************************************************************************#
# (g) Plot profile plot of centroids for the six clusters generated in (f). Copy the code used and the result below.
plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = c(min(km2$centers), max(km2$centers)), xlim = c(0, 8))
axis(1, at = c(1:4), labels = names(dungaree.df))

for (i in c(1:6))
  lines(km2$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),
                                                        "black", "dark grey"))

text(x = 0.2, y = km2$centers[, 1], labels = paste("Cluster", c(1:6)))
km2$centers
#******************************************************************************************************************************#







# 2.
#******************************************************************************************************************************#
# (a) Import the data to R, set row names to the “Symbol” column, and remove all the columns that you are not going to use for clustering. 
# Copy the R code used below.
Pharmaceuticals.df <- read.csv("Pharmaceuticals.csv")
row.names(Pharmaceuticals.df) <- Pharmaceuticals.df[,1]
Pharmaceuticals.df <- Pharmaceuticals.df[,c(3:11)]
#******************************************************************************************************************************#



#******************************************************************************************************************************#
# (b) Normalize the data. Copy the R code used below.

Pharmaceuticals.df.norm <- sapply(Pharmaceuticals.df, scale)
row.names(Pharmaceuticals.df.norm) <- row.names(Pharmaceuticals.df)
#******************************************************************************************************************************#



#******************************************************************************************************************************#
# (c) Based on single linkage, run hierarchical clustering to generate Dendrogram. Copy the code used and the result below.

d.norm <- dist(Pharmaceuticals.df.norm, method = "euclidean")
hc1.norm <- hclust(d.norm, method = "single")
plot(hc1.norm, hang = -1)
#******************************************************************************************************************************#



#******************************************************************************************************************************#
# (d) If we are interested in 6 clusters based on Dendrogram in (c), what are the members of each cluster? Copy the code used and the result below.
members <- cutree(hc1.norm, k = 6)
members
#******************************************************************************************************************************#




#******************************************************************************************************************************#
# (e) Based on complete linkage, run hierarchical clustering to generate Dendrogram. Copy the code
# used and the result below.

hc2.norm <- hclust(d.norm, method = "complete")
plot(hc2.norm, hang = -1)

#******************************************************************************************************************************#




#******************************************************************************************************************************#
# (f) If we are interested in 6 clusters based on Dendrogram in (e), what are the members of each
# cluster? Copy the code used and the result below.

memberc <- cutree(hc2.norm, k = 6)
memberc
#******************************************************************************************************************************#

