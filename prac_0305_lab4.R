# Lab 
# March 5
# Name: Yueyang Li

## Heatmap, image, and hierarchical clustering
set.seed(12345)
par(mar = rep(0.2, 4))

# randomly generate values
data_Matrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

# hierarchical clustering, using heatmap() function
par(mar = rep(0.2, 4))
heatmap(data_Matrix)

# add a pattern to the data (random rows)
set.seed(678910)
for(i in 1:40) {
  coin <- rbinom(1, size = 1, prob = 0.5)
  if(coin) {
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0, 3), each = 5)
  }
}

# plot the data
# right: mean of 3
# left: mean of 0
par(mar = rep(0.2, 4))
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

# hierarchical clustering
# seperate two sets of columns (0 / 3)
par(mar = rep(0.2, 4))
heatmap(data_Matrix)

# take a closer look at the patterns in rows and columns
# hclust(): Hierarchical Clustering
# hh$order: a vector giving the permutation of the original observations suitable for plotting, 
#           in the sense that a cluster plot using this ordering and matrix merge will not have crossings of the branches.
hh <- hclust(dist(data_Matrix))
data_Matrix_ordered <- data_Matrix[hh$order, ]
# plot
par(mfrow = c(1, 3))
image(t(data_Matrix_ordered)[, nrow(data_Matrix_ordered):1])
plot(rowMeans(data_Matrix_ordered), 40:1, xlab = "The Row Mean", ylab = "Row", pch = 19)
plot(colMeans(data_Matrix_ordered), xlab = "Column", ylab = "The Column Mean", pch = 19)
