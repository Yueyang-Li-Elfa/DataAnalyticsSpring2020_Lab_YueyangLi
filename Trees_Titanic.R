# Trees for the Titanic

# Get the data
library(titanic)
data("titanic_train")
head(titanic_train)
summary(titanic_train)

# Age: has 177 missing values.
# impute by mean
titanic_train$Age <- ifelse(is.na(titanic_train$Age), mean(titanic_train$Age, na.rm = T), titanic_train$Age)

# rpart
library(rpart)
library(rpart.plot)
model1 <- rpart(Survived ~ Sex + Age + Pclass, data = titanic_train, method = "class")
summary(model1)
rpart.plot(model1, type = 3, fallen.leaves = T)
# prediction
pred1 <- predict(model1, titanic_train, type = "class")
# confusition matrix
table(actual = titanic_train$Survived, pred = pred1)

# ctree
library(C50)
titanic_train$Survived <- as.factor(titanic_train$Survived)
model2 <- C5.0(Survived ~ Sex + Age + Pclass, data = titanic_train)
summary(model2)
plot(model2)
# prediction
pred2 <- predict(model2, titanic_train, type = "class")
# confusition matrix
table(actual = titanic_train$Survived, pred = pred2)

# random forest
library(randomForest)
# random forest cannot deal with characters
# conver to factor
titanic_train$Sex <- as.factor(titanic_train$Sex)
model3 <- randomForest(Survived ~ Sex + Age + Pclass, data = titanic_train, importance = T)
summary(model3)
# prediction
pred3 <- predict(model3, titanic_train, type = "class")
# confusition matrix
table(actual = titanic_train$Survived, pred = pred3)