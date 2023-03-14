#https://r-charts.com/part-whole/circular-dendrogram/
rm(list=ls())
library(ape)


# set seed for reproducibility
set.seed(123)
rand_matrix <- matrix(rpois(200, 3), nrow = 50, ncol = 4)
# create a hierarchical clustering object
# hc <- hclust(dist(USArrests), "ward.D2")
row_names<-paste0('variable',1:nrow(rand_matrix))
row.names(rand_matrix)<-row_names
hc <- hclust(dist(rand_matrix), "ward.D2")

# create a star tree dendrogram plot
plot(as.phylo(hc), type = "fan", cex = 0.6, edge.width = 0.5, edge.col = "gray")
## cluster fixing
plot(hc)
abline(h = mean(dist(rand_matrix[1:50,2:3])), col = 'red')
abline(h = 1000, col = 'red')   ## nearest 0

# color fixing 
# Cut the dendrogram into 5 clusters
colors = c("red", "blue", "green", "black",'magenta')
clus4 = cutree(hc, 5)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus4],
     label.offset = 1, cex = 0.7)
