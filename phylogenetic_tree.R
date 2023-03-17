library(ape) # for phylogenetic tree analysis
library(phangorn) # for maximum likelihood phylogenetic tree inference


# Generate random data
set.seed(123)
n <- 10
m <- matrix(rnorm(n^2), nrow = n)
rownames(m) <- paste0("sample", 1:n)
colnames(m) <- paste0("gene", 1:n)

# Compute distance matrix
d <- dist(m)

# Create tree object using neighbor joining method
tree <- nj(d)

# Plot tree
plot(tree)
################################# with node 

library(ape)

# Create a matrix of random data with 10 rows and 5 columns
set.seed(123)
mat <- matrix(rnorm(10 * 5), nrow = 10, ncol = 5)
rownames(mat) <- paste0("sample", 1:10)
colnames(mat) <- paste0("gene", 1:5)

# Compute distance matrix using Jukes-Cantor model
dist_mat <- dist.dna(mat, model = "JC")

# Build neighbor joining tree
tree <- nj(dist_mat)

# Create a data frame of metadata for each sample
metadata <- data.frame(
  sample = rownames(mat),
  location = c("A", "B", "C", "D", "E", "A", "B", "C", "D", "E"),
  value = rnorm(10, mean = 5, sd = 1)
 
)

# Add metadata to the tree
tree$tip.label <- metadata$sample
tree$node.label <- metadata$value

# Create a function to plot a figure in a node
plot_figure <- function(node, metadata) {
  if (isTip(node)) return()  # don't plot figures for tips
  x <- node$edge.length / 2  # x position of figure
  y <- max(node$edge[, 2]) + 0.2  # y position of figure
  sample_names <- getTips(node)
  values <- metadata[metadata$sample %in% sample_names, "value"]
  height <- max(values) - min(values)
  width <- height / 2
  plot(x, y, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(x - width, x + width), ylim = c(y, y + height))
  points(rep(x, length(values)), values, pch = 20)
  abline(h = mean(values))
}

# Plot the tree with lable  in the nodes
plot(tree, main = "Phylogenetic Tree with lable in Nodes")
tiplabels(pch = 20, cex = 1.2)
nodelabels(pch = 20, cex = 1.2)
tiplabels(metadata$location, pch = 16, col = "red", adj = c(1, 0.5))
nodelabels(round(tree$node.label,2), frame = "none", col = "blue", adj = c(0.5, 1))
#tiplabels(metadata$sample, col = "green", adj = c(0, 1))





