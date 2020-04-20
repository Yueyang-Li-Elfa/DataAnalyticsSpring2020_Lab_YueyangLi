# In-class practice
# Apr 20 

## LOWESS example using the Cars dataset
data(cars)
str(cars) # 50 * 2

# plot: speed vs distance
plot(speed ~ dist, data = cars)
# We can see a positive relationship between them

# the documentation for the lowess function
?lowess

# use the lowess() function: return values
lowess(cars$speed ~ cars$dist)
# combine with lines() function
lines(lowess(cars$speed ~ cars$dist, f = 2/3), col = "blue") # default f value

# change the f value
# larger values give more smoothness
lines(lowess(cars$speed ~ cars$dist, f = 0.8), col = "red")
lines(lowess(cars$speed ~ cars$dist, f = 0.9), col = "green")
lines(lowess(cars$speed ~ cars$dist, f = 0.1), col = 5)
lines(lowess(cars$speed ~ cars$dist, f = 0.01), col = 6) # almost connect each point, overfit


## Linear Discriminant Analysis example using Iris dataset
library(MASS)
names(iris)
dim(iris)
head(iris)

# train test split
set.seed(100)
index <- sample(1:nrow(iris), nrow(iris)/2)
iris_train <- iris[index, ]
iris_test <- iris[-index, ]

# the documentation of lda() function
?lda

# use lda() function to fit the model
fit1 <- lda(Species ~ ., data = iris_train)

# predict on training data
predict1 <- predict(fit1, iris_train)
predict1_class <- predict1$class
# confusion matrix
table1 <- table(predict1_class, iris_train$Species)
table1
# accuracy
sum(diag(table1)) / sum(table1)

# predict on testing data
predict2 <- predict(fit1, iris_test)
predict2_class <- predict2$class
# confusion matrix
table2 <- table(predict2_class, iris_test$Species)
table2
# accuracy
sum(diag(table2)) / sum(table2)
