# Inclass Mar 30
# Yueyang Li

# SVM Classification example
library(ggplot2)
library(e1071)

data("iris")
iris %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species))

# -------- Model 1 --------
# build in svm() function, e1071
?svm
svm_model1 <- svm(Species ~ ., data = iris)
summary(svm_model1)

# plot the result
plot(svm_model1, data = iris, Petal.Width~Petal.Length, 
     slice = list(Sepal.Width = 3, Sepal.Length = 4))

# prediction, and create the table
pred1 <- predict(svm_model1, iris)
table1 <- table(Predicted = pred1, Actual = iris$Species)

# accuracy: manually
sum(diag(table1))/sum(table1)
# misclassification rate
1 - sum(diag(table1))/sum(table1)

# ------- Model 2 --------
# kernal = "linear"
svm_model2 <- svm(Species ~ ., data = iris, kernel = "linear")
summary(svm_model2)

# plot the result
plot(svm_model2, data = iris, Petal.Width~Petal.Length, 
     slice = list(Sepal.Width = 3, Sepal.Length = 4))

# prediction and table
pred2 <- predict(svm_model2, iris)
table2 <- table(Predicted = pred2, Actual = iris$Species)

# accuracy: manually
sum(diag(table2))/sum(table2)
# misclassification rate
1 - sum(diag(table2))/sum(table2)

# ------- Model 3 --------
# kernal = "polynomial"
svm_model3 <- svm(Species ~ ., data = iris, kernel = "polynomial")
summary(svm_model3)

# plot the result
plot(svm_model3, data = iris, Petal.Width~Petal.Length, 
     slice = list(Sepal.Width = 3, Sepal.Length = 4))

# prediction and table
pred3 <- predict(svm_model3, iris)
table3 <- table(Predicted = pred3, Actual = iris$Species)

# accuracy: manually
sum(diag(table3))/sum(table3)
# misclassification rate
1 - sum(diag(table3))/sum(table3)
