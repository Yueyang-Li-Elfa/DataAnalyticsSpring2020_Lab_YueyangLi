# Group 3 Lab 4
# Apr 2
# Yueyang Li

library(e1071)
set.seed(1)

# SVM: 2-dimensional example
# Generating observations
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1,] <- x[y == 1, ] + 1

# Check: linearly separable?
plot(x, col = (3-y))
# They are not.

# SVM requires the response to be a factor
dat <- data.frame(x = x, y = as.factor(y))
dat

# Fit SVM
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = F)

# Plot the support vector classifier
plot(svmfit, data = dat)
# x: support vectors
# determine the identities of support vectors
svmfit$index
# basic information
summary(svmfit)

# Try cost = 0.1
svmfit_new <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = F)
plot(svmfit_new, data = dat)
# Smaller cost, more support vectors, wider margin

# Perform cross-validation using tune() to find the optimal cost
# default k = 10
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
# cost = 0.1 has the lowest cross-validation error

# The best model obtained
bestmodel <- tune.out$best.model
summary(bestmodel)

# Generate the test set
xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = T)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

# Predict labels of test observations
ypred <- predict(bestmodel, testdat)
table(predict = ypred, actual = testdat$y)

# Change to model with cost = 0.01
svmfit_lowcost <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.01, scale = F)
ypred2 <- predict(svmfit_lowcost, testdat)
table(predict = ypred2, actual = testdat$y)
# not as good

# Consider a situation: two classes are linearly separable
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (y+5)/2)
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1e5, scale = F)
summary(svmfit)
plot(svmfit, dat)
# only uses 3 support vectors
# the margin is so narrow that it seems like this model will not perform well on new data
# try a smaller cost
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1, scale = F)
summary(svmfit)
plot(svmfit, dat)
# now it uses 7 support vectors