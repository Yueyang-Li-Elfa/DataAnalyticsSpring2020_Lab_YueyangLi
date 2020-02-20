# Lab 3
# Feb 20
# Name: Yueyang Li

library(rpart)
library(rpart.plot)
library(tidyverse)

# load msleep data
data("msleep")
str(msleep)
str(data)

# read the documentation
?msleep

# Create new dataframe
msleepDF1 <- msleep[, c(3, 6, 10, 11)]
str(msleepDF1)
head(msleepDF1)

# read the documentation: rpart
?rpart

# Building a regression tree
sleepModel_1 <- rpart(sleep_total ~., data = msleepDF1, method = "anova")

# visualize the model using rpart.plot()
?rpart.plot
rpart.plot(sleepModel_1, type = 3, fallen.leaves = T)
# try different arguments values
rpart.plot(sleepModel_1, type = 3, fallen.leaves = T, digits = 3)
rpart.plot(sleepModel_1, type = 3, fallen.leaves = T, digits = 4)
rpart.plot(sleepModel_1, type = 3, fallen.leaves = F)


## classification tree, Ctree()
# install package C50
#install.packages("C50")
require(C50)

# iris dataset
data("iris")
head(iris)
str(iris)
table(iris$Species)

# shuffle
set.seed(9850)
grn <- runif(nrow(iris))  # between 0 and 1
irisrand <- iris[order(grn),]  # from 1 to 150 in random order
str(irisrand)

# Build the classification tree model
classification_model1 <- C5.0(irisrand[1:100, -5], irisrand[1:100, 5])
classification_model1
summary(classification_model1)

# Predict
prediction1 <- predict(classification_model1, irisrand[101:150,])
# confusion matrix
table(irisrand[101:150, 5], prediction1)
# Plotting the classification tree
plot(classification_model1)

# also, read the documentation of ctree()
?ctree

## NaiveBayes Classifier
library(e1071)
classifier <- naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[, -5]), iris[, 5],
      dnn = list('predicted', 'actual'))

# Class distribution for the dependent variable.
classifier$apriori

# Mean and sd of the variable, for each class
classifier$tables$Petal.Length
# Plot the distribution
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")