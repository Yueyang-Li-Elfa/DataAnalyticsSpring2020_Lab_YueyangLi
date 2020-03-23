# Inclass practice
# Name: Yueyang Li

## PCA, USArrests
data("USArrests")

# rownames
states <- row.names(USArrests)
states

# colnames
names(USArrests)

# apply mean / var function to each column
# 2: by col
# 1: by row
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# perform PCA using prcomp()
pr.out <- prcomp(USArrests, scale. = T)
names(pr.out)

# means and sds of variables
# same as result of previous apply function
pr.out$center
pr.out$scale

# loadings
pr.out$rotation

# principal component score vector
pr.out$x

# plot the first two pcs
# scale = 0 means the center of each axis is 0
biplot(pr.out, scale = 0)

# sds of each pc
pr.out$sdev

# compute the propotion of variance explained by each pc
pr.var <- pr.out$sdev ^ 2
pve <- pr.var / sum(pr.var)
pve


## ======================================
## PCA, Iris
data("iris")
head(iris)

# subset: col 1:4
irisdata1 <- iris[, 1:4]
head(irisdata1)

# perform PCA
# cor: the calculation should use correlation matrix (or covariance matrix)
# scores: calculate score on each PC
PCA_iris <- princomp(irisdata1, cor = T, scores = T)
summary(PCA_iris)

# plot the components
plot(PCA_iris)
plot(PCA_iris, type = "l")
biplot(PCA_iris)


## =====================================
## PCA, Boston

# get the data
data(Boston, package = "MASS")

# perform PCA
# scale.: a logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place
PCA_Boston <- prcomp(Boston, scale. = T)
PCA_Boston

# plot the result
plot(PCA_Boston)
biplot(PCA_Boston, scale = 0)

# summary of each pc
summary(PCA_Boston$x)