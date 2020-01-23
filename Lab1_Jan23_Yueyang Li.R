# Jan 23
# Lab 1
# Name: Yueyang Li

# data.frame
# distributions / populations
# fitting
# filtering

# Creating a dataframe
days <- c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
temp <- c(28, 30.5, 29, 31, 32, 29.3, 26.4)
snowed <- c("T", "T", "F", "F", "T", "T", "F")
RPI_Weather_Week <- data.frame(days, temp, snowed)

head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

# subset
RPI_Weather_Week[1, ] # row
RPI_Weather_Week[, 1] # col

RPI_Weather_Week[, 'days']  # by name
RPI_Weather_Week$days  # by name
subset(RPI_Weather_Week, subset = snowed == "T")

# subset & order
sourted_snowed <- order(RPI_Weather_Week$snowed)
RPI_Weather_Week[sourted_snowed, ]

dec_temp <- order(-RPI_Weather_Week$temp)
RPI_Weather_Week[dec_temp, ]

# Creating an empty dataframe
Empty_DataFrame <- data.frame()

# Exporting data
# Writing 
write.csv(RPI_Weather_Week, file = "RPI_Weather_Week.csv")

# ===============================
# Exercise: reading data

library(readxl)
library(tidyverse)

# read in data

# read in csv
GPW3_GRUMP_2010 <- read.csv("data/GPW3_GRUMP_SummaryInformation_2010.csv")
EPI_2010 <- read_csv("data/2010EPI_data.csv", skip = 1)
 # note: need to skip the first line

# just for fun, try to read in an excel file
# read in only "EPI2010_all countries" sheet
EPI2010_all_countries <- read_xls("data/2010EPI_data.xls", sheet = 5)

head(GPW3_GRUMP_2010) # check if it reads correctly
str(GPW3_GRUMP_2010) # Area is a factor? wired

head(EPI_2010)
str(EPI_2010) 
  # some cols are mistaken as characters, like Population07
  # some factors are mistaken as numeric valus, like Desert

head(EPI2010_all_countries) 
str(EPI2010_all_countries) # some cols are mistaken as characters


# Plot some variables

EPI_2010 %>%
  ggplot(aes(x = EPI)) +
  geom_histogram()
  # seems symmetrical

EPI_2010 %>%
  ggplot(aes(x = Landarea, y = EPI)) +
  geom_point(aes(color = EPI_regions)) +
  scale_x_log10() 
  # not a good one. too many classes

EPI_2010 %>%
  ggplot(aes(group = Desert, y = EPI)) +
  geom_boxplot()
  # it works, but better haddle with data type first

# ================================

attach(EPI_2010)
fix(EPI_2010)   # error: R version problem, save for later

EPI
tf <- is.na(EPI)
E <- EPI[!tf]  # filter out NA


# ================================
# Exercise 1: exploring the distribution

summary(EPI)
fivenum(EPI, na.rm = T)   # by default, na.rm = T
stem(EPI)  # stem and leaf plot
hist(EPI)
hist(EPI, breaks = seq(30., 95., 1.0), prob = T) # breakpoints

# add lines to previous histogram
lines(density(EPI, na.rm = T, bw = 1.))
lines(density(EPI, na.rm = T, bw = "SJ"))
  # bw: bandwidth (Kernel Density Estimation)
  # "SJ": the methods of Sheather & Jones (1991) to select the bandwidth using pilot estimation of derivatives.

# add rugs (at the bottom of the plot)
rug(EPI)

# cumulative density function
plot(ecdf(EPI), do.points = F, verticals = T)

# ========== class ends