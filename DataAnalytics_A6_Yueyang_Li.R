# Data Analytics Term Project
# Stanford Open Policing Project: Traffic stops in Illinois
# Name: Yueyang Li
# Level: 6000

## Important note:
## The training process will take about 7-8 hours

library(tidyverse)
library(lubridate)

# Read in data
IL <- read_csv("data/IL.csv", col_types = cols(stop_duration = "n"))
head(IL)
summary(IL)

## Examine each variable
## Clean the data: delete useless/duplicated variables 
## *click the button below to unfold code
## =====================================================

# id: index --> delete
# state: all identical (IL) --> delete
IL %>% count(state)

# Time information
# Variables: stop_date, stop_time
# Need to extract month, weekday, hour --> save for later

# Location information
# can be provided from a single variable "fine_grained_location"
# location_raw, district: identical and no additional information --> delete
identical(IL$location_raw, IL$district)
# county_name, county_fips: no additional information was provided --> delete
IL %>% count(fine_grained_location, county_name, county_fips)
# police_department: all identical --> delete
IL %>% count(police_department)
# fine_grained_location: still has too many unique values (24)
# cannot be used directly --> save for later

# Driver and car information
# This dataset has been transformed and tidied
# Useful variables: gender, age, race
# Duplicated variables: age_raw, race_raw --> delete
# vehicle_type: has 29255 unique values, can not be used directly --> save for later
IL %>% count(vehicle_type)

# Stop information
# Useful variables: violation, search_conducted, contraband_found, stop_duration
# Duplicated variables: violation_raw, search_type_raw --> delete
# search_type: only available when a search was conducted --> save for later
# is_arrested: all NA --> delete
IL %>% count(is_arrested)
# stop_duration: negative values? NAs? --> save for later
summary(IL$stop_duration)
# drug_related_stop: 50% missing values --> delete
IL %>% count(drugs_related_stop)

# Select useful variables
# Delete variables providing duplicated information or no information
IL <- IL %>%
  select(stop_date, stop_time, fine_grained_location,
         driver_gender, driver_age, driver_race, vehicle_type,
         violation, search_conducted, search_type, contraband_found, stop_duration,
         stop_outcome)

# Location and vehicle information is hard to use directly
# Search type is dependent on search_conducted, save for later
# Create a new smaller dataset
IL_small <- IL %>%
  select(-fine_grained_location, -vehicle_type, -search_type)

## ==========================================================
## Other Transformation
## Check on missing value (imputation: after train test split)
## Extract new variables: month, weekday, hour
## Any anomaly?
## *click the button below to unfold code
## ==========================================================

# Missing Value
map(modify(IL_small, is.na), sum)

# Imputation idea
# stop_time: 2331 -- extract hour then impute hour (median)
# driver_age: 2932 (mean)
# stop_duration: 1286028 (mean)

# Extract new variables: year, month, weekday, hour
IL_small <- IL_small %>%
  mutate(year = year(stop_date),
         month = month(stop_date),
            weekday = weekdays(stop_date),
            hour = hour(stop_time)) %>%
  select(-stop_date, -stop_time)

# Convert characters to factors
IL_small <- modify_if(IL_small, is.character, as.factor)
IL_small <- IL_small %>% mutate(month = as.factor(month),
                                weekday = factor(weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

# Anomaly?
summary(IL_small)
# stop_duration: negative value --> replace with NA
IL_small <- IL_small %>%
  mutate(stop_duration = ifelse(stop_duration < 0, NA, stop_duration))
# check again
IL_small %>% filter(stop_duration < 0)

## ==========================================================
## Analysis
## Distribution of target variable
## Relationship with the target: stop outcome
## *click the button below to unfold code
## ==========================================================

# Stop outcome: distribution --> imbalanced
IL_small %>% count(stop_outcome) %>% mutate(percent = n / sum(n))
IL_small %>%
  count(stop_outcome) %>%
  mutate(percentage = n/sum(n)) %>%
  ggplot(aes(x = stop_outcome, y = percentage)) +
  geom_col()

# Stop duration: seperate into 2 groups
# Normal length: equal or less than one hour
IL_small %>%
  filter(stop_duration <= 60) %>%
  ggplot(aes(x = stop_outcome, y = stop_duration)) +
  geom_boxplot()
# Long stop: higher than one hour
IL_small %>%
  filter(stop_duration > 60) %>%
  ggplot(aes(x = stop_outcome, y = stop_duration)) +
  geom_boxplot()
# plots are not reader friendly
# use numbers instead
IL_small %>%
  group_by(stop_outcome) %>%
  summarize(mean = mean(stop_duration, na.rm = T),
            median = median(stop_duration, na.rm = T),
            sd = sd(stop_duration, na.rm = T))

# Gender
IL_small %>%
  ggplot(aes(x = stop_outcome, fill = driver_gender)) +
  geom_bar(position = "fill") +
  ylab("percentage")

# Age
IL_small %>%
  ggplot(aes(x = stop_outcome, y = driver_age)) +
  geom_boxplot()
# Young drivers are more likely to get citation or written warning
# While drivers who receive verbal warning are older than other two groups

# Race
IL_small %>%
  ggplot(aes(x = driver_race, fill = stop_outcome)) +
  geom_bar(position = "fill")

# Violation
IL_small %>%
  ggplot(aes(x = violation, fill = stop_outcome)) +
  geom_bar(position = "fill") +
  coord_flip() +
  ylab("percentage")

# Verbal Warning: other violation
# Citation: seat belt, moving violation, speeding
# Written Warning: safe movement (?)

# Search conducted
IL_small %>%
  ggplot(aes(x = search_conducted, fill = stop_outcome)) +
  geom_bar(position = "fill") +
  ylab("percentage")
# If a search was conducted, then more chance to get a citation

# Contraband found
IL_small %>%
  count(search_conducted, contraband_found)
# When a search was conducted
IL_small %>%
  filter(search_conducted == T) %>%
  ggplot(aes(x = contraband_found, fill = stop_outcome)) +
  geom_bar(position = "fill") +
  ylab("percentage")
# If contraband was found, then more likely to get a citation

# year
IL_small %>%
  ggplot(aes(x = year, fill = stop_outcome)) +
  geom_bar(position = "fill") +
  ylab("percentage")

# month
IL_small %>%
  ggplot(aes(x = month, fill = stop_outcome)) +
  geom_bar(position = "fill") +
  ylab("percentage")

# weekday
IL_small %>%
  ggplot(aes(x = weekday, fill = stop_outcome)) +
  geom_bar(position = "fill") +
  ylab("percentage")

# hour: daytime will yield more citations
IL_small %>%
  ggplot(aes(x = hour, fill = stop_outcome)) +
  geom_bar(position = "fill") +
  ylab("percentage")

# It's more meaningful to replace "hour" with "daytime"
# daytime: 6:00 AM - 6:00 PM
IL_small <- IL_small %>%
  mutate(daytime = ifelse(hour >= 6 & hour < 18, "daytime", "nightime")) %>%
  select(-hour)


## ==========================================================
## Preparation before modeling
## train test split
## impute missing values for the training data
## under-sampling
## *click the button below to unfold code
## ==========================================================

# train test split
set.seed(666)
index <- sample(nrow(IL_small), nrow(IL_small)*0.6)
train.all <- IL_small[index, ]
test <- IL_small[-index, ]

# impute missing values for the training data
# daytime: use the more common class "daytime"
count(train.all, daytime)
# driver_age: mean
# stop_duration: mean
train.all <- train.all %>%
  mutate(daytime = as.factor(ifelse(is.na(daytime), "daytime", daytime)),
         driver_age = ifelse(is.na(driver_age), mean(driver_age, na.rm = T), driver_age),
         stop_duration = ifelse(is.na(stop_duration), mean(stop_duration, na.rm = T), stop_duration))

# under-sampling: construct a balanced train set
# choose under-sampling because we have enough data
# keep all minority cases: 35924 Verbal Warning
count(train.all, stop_outcome)
train.minority <- train.all %>% filter(stop_outcome == "Verbal Warning")
train.citation <- train.all %>% filter(stop_outcome == "Citation")
train.written <- train.all %>% filter(stop_outcome == "Written Warning")
set.seed(666)
index.citation <- sample(nrow(train.citation), 35924)
set.seed(666)
index.written <- sample(nrow(train.written), 35924)
train.resample <- rbind(train.minority, train.citation[index.citation, ], train.written[index.written, ])
# check the distribution of resampled train set
summary(train.resample)

## ==========================================================
## Modeling 
## *click the button below to unfold code
## ==========================================================
library(caret)

# Run algorithms using 5-fold cross validation
control <- trainControl(method = "cv", number = 5)
metric <- "Kappa"

# Build models
# 1. LDA (using MASS package)
set.seed(666)
fit.lda <- train(stop_outcome ~., data = train.resample, method = "lda", metric = metric, 
                 trControl = control, tuneLength = 10)

# 2. Decision Tree (using rpart package)
set.seed(666)
fit.tree <- train(stop_outcome ~., data = train.resample, method = "rpart", metric = metric, 
                  trControl = control, tuneLength = 10)
# 3. Random Forest (using randomForest package)
set.seed(666)
fit.rf <- train(stop_outcome ~., data = train.resample, method = "rf", metric = metric, 
                trControl = control, tuneLength = 10)

# Interpretation
# summarize model performance
results <- resamples(list(LinearDiscriminantAnalysis = fit.lda,
                          DecisionTree = fit.tree,
                          RandomForest = fit.rf))
summary(results)
# compare model performance
dotplot(results)

# impute test data
test <- test %>%
  mutate(daytime = as.factor(ifelse(is.na(daytime), "daytime", daytime)),
         driver_age = ifelse(is.na(driver_age), mean(driver_age, na.rm = T), driver_age),
         stop_duration = ifelse(is.na(stop_duration), mean(stop_duration, na.rm = T), stop_duration))
summary(test)

# Generate prediction using LDA model
pred.lda.train <- predict(fit.lda, train.resample)
confusionMatrix(pred.lda.train, train.resample$stop_outcome)

# Plot the Decision Tree
#library(rpart.plot)
#rpart.plot(prune.ct)
printcp(fit.tree$finalModel)
prune.ct <- prune(fit.tree$finalModel, cp = 0.0067364)
prp(prune.ct,type=1,extra=1,under=TRUE,split.font=2,varlen=-10,
    box.col=ifelse(prune.ct$frame$var=="<leaf>","gray","white"))

# Generate prediction using decision tree model
pred.tree.train <- predict(fit.tree, train.resample)
confusionMatrix(pred.tree.train, train.resample$stop_outcome)

# Feature importance from random forest
library(randomForest)
fit.rf$finalModel$importance
varImpPlot(fit.rf$finalModel, type = 1, main = "")

# Generate prediction using random forest model
pred.rf.train <- predict(fit.rf, train.resample)
confusionMatrix(pred.rf.train, train.resample$stop_outcome)


## ==========================================================


