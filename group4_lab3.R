# Group 4 Lab 3
# Apr 27
# Name: Yueyang Li

library(caret)

# load the iris data
data(iris)

# rename the dataset
dataset <- iris
head(dataset)

# creat a 80% partition
train_index <- createDataPartition(dataset$Species, p = 0.8, list = F)
# the rest 20% as validation
validation <- dataset[-train_index, ]
# the remaining 80% as training and testing set
dataset <- dataset[train_index, ]
dim(dataset)

# list types for each attribute
sapply(dataset, class)
# list the levels for the class
levels(dataset$Species)
# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq = table(dataset$Species), percentage = percentage)

# summarize attribute distributions
summary(dataset)

# split input and output
x <- dataset[, 1:4]
y <- dataset[, 5]

# boxplot for each attribute on one image
par(mfrow = c(1, 4))
for (i in 1:4) {
  boxplot(x[, i], main = names(iris)[i])
}

# barplot for class breakdown
par(mfrow = c(1, 1))
plot(y)

# Multivariate Plots
# scatterplot matrix
featurePlot(x = x, y = y, plot = "ellipse")
# box and whisker plots for each attribute
featurePlot(x = x, y = y, plot = "box")
# density plots for each attribute by class value
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = x, y = y, plot = "density", scales = scales)

# Run algorithms using 10-fold cross validation
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"

# Build Models
# a) linear algorithm
set.seed(7)
fit.lda <- train(Species ~., data = dataset, method = "lda", metric = metric, trControl = control)
# b) nonlinear algorithm
# CART: decision tree
set.seed(7)
fit.cart <- train(Species ~ ., data = dataset, method = "rpart", metric = metric, trControl = control)
# kNN
set.seed(7)
fit.knn <- train(Species ~ ., data = dataset, method = "knn", metric = metric, trControl = control)
# c) advanced algorithm
# SVM
set.seed(7)
fit.svm <- train(Species ~ ., data = dataset, method = "svmRadial", metric = metric, trControl = control)
# Random Forest
set.seed(7)
fit.rf <- train(Species ~ ., data = dataset, method = "rf", metric = metric, trControl = control)

# Interpretation
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
# compare accuracy of models
dotplot(results)
# summarize Best Model
print(fit.lda)
# estimate skill of LDA on the validation set
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
