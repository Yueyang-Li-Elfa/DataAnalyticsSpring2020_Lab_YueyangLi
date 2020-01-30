# Lab 1 Part 2, Jan 30
# Name: Yueyang Li

# A. dplyr
# B. distributions, correlations, Linear Regression
# C. creating plots using ggplot2

# ===================================
## A. dplyr

library(tidyverse)
library(nycflights13)

head(flights)  # tibble
#summary(flights)

# 1. filter(): select rows
flights %>%
  filter(month == 11, day == 18, carrier == "AA")

# 2. slice(): select rows
flights %>%
  slice(1:10)

# 3. arrange(): sort rows
flights %>%
  arrange(dep_time)

flights %>%
  arrange(desc(dep_time))

# 4. select(): select cols
flights %>%
  select(year, month, day, dep_time, arr_time)

# 5. distinct(): find distinct values in a column
flights %>%
  distinct(carrier)  # 16 carriers

# 6. mutate(): create new cols based on existing cols
flights %>%
  mutate(makeup = arr_delay - dep_delay)

flights %>%
  transmute(makeup = arr_delay - dep_delay)

# 7. summarise(): aggregation
#    usually used together with group_by()
flights %>%
  summarize(avg_air_time = mean(air_time, na.rm = T))

flights %>%
  group_by(carrier) %>%
  summarize(avg_air_time = mean(air_time, na.rm = T))

# 8. sample_n(): pick n random rows
flights %>%
  sample_n(30)

# 9. sample_frac(): pick n% random rows
flights %>%
  sample_frac(0.1)

# pipe operator
results_mpg <- mtcars %>%
  filter(mpg > 20) %>%
  sample_n(10) %>%
  arrange(desc(mpg))

# =====================================================
## B. distributions, correlations, Linear Regression

# read in data
EPI_2010 <- read_csv("data/2010EPI_data.csv", skip = 1)

## Exercise 1: fitting a distribution
# cumulative density function
plot(ecdf(EPI_2010$EPI), do.points = F, verticals = T)

# Quantile-Quantile plot
par(pty = "s")  # make the plot a square
qqnorm(EPI_2010$EPI)
qqline(EPI_2010$EPI)

# make a Q-Q plot against the generating distribution 
x1 <- seq(30, 95, 1)  # from 30 to 95 by 1
x2 <- qt(ppoints(250), df = 5)  
  # qt(): the Student t Distribution
  # ppoints(): generate the sequence of probability points
qqplot(x2, x1, xlab = "Q-Q plot for t dsn")
qqline(x1)
  # obviously, different distributions

# try other variables: DALY
plot(ecdf(EPI_2010$DALY), do.points = F, verticals = T)
qqnorm(EPI_2010$DALY)
qqline(EPI_2010$DALY)
hist(EPI_2010$DALY)   # dense in higher values

# try other variables: WATER_H
plot(ecdf(EPI_2010$WATER_H), do.points = F, verticals = T)
qqnorm(EPI_2010$WATER_H)
qqline(EPI_2010$WATER_H)
hist(EPI_2010$WATER_H)   # dense in higher values

qqplot(EPI_2010$DALY, EPI_2010$EPI)
qqplot(EPI_2010$WATER_H, EPI_2010$EPI)

# comparing distributions
boxplot(EPI_2010$EPI, EPI_2010$DALY)
boxplot(EPI_2010$EPI, EPI_2010$WATER_H)
  # DALY, WATER_H: more scattered
  # DALY: same median, lower values
  # WATER_H: higher median, much more high values

# more to go...
attach(EPI_2010)
boxplot(EPI, ENVHEALTH)   # higher median, more scattered
boxplot(EPI, ECOSYSTEM)   # almost same
boxplot(EPI, AIR_H)       # higher median, more scattered
boxplot(EPI, BIODIVERSITY) # slightly higher median, lower values
detach(EPI_2010)

## 2: Linear Regression & corr
# read in data
multivriate <- read_csv("data/multivariate.csv")

head(multivriate)
attach(multivriate)

cor(Homeowners, Immigrant)  # slight negative linear association
mm <- lm(Homeowners ~ Immigrant)
summary(mm)
attributes(mm)

# extract elements from the model
mm$coefficients
mm$residuals
plot(mm$residuals)

plot(Homeowners ~ Immigrant)
abline(mm, col = 4, lwd = 1.5)

detach(multivriate)

# =====================================================
# C. creating plots using ggplot2

# when using base R
plot(mtcars$wt, mtcars$mpg)

# ggplot2
qplot(data = mtcars, wt, mpg)
mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_point()

pressure %>%
  ggplot(aes(temperature, pressure)) +
  geom_line() +
  geom_point()

# bar plot
mtcars %>%
  ggplot(aes(x = factor(cyl))) +
  geom_bar()

# histogram
mtcars %>%
  ggplot(aes(x = mpg)) +
  geom_histogram(binwidth = 5)
