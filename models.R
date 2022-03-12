library(mice)
library(dplyr)
library(tidyr)
setwd("~/Escritorio/Time Series Data Mining/
      Productivity-Prediction-of-Garment-Employees-Data-Set")

# read data
data<- read.csv("data/garments_worker_productivity.csv")

head(data)
## Basic preprocessing:
# cite: https://www.r-bloggers.com/2021/04/handling-missing-values-in-r/ 

# Eliminate NA's 
colSums(is.na(data))
dim(data)
# percentage of missing value
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(data, 2, p)

# We will try two approaches, deleting "wip" column and dealing with the missing
# values! Deleting rows with missing values, leads to a reduction in sample size
# and avoid some good representative values also. We won't do that, since we 
# would be deleting almost half of the instances. 

data_no_wip <- subset( data, select = -wip )

# Data Imputation methods 
impute <- mice(data, seed = 123) #estimates NA values 5 times by default
print(impute) # pmm method: predictive Mean Matching
newDATA <- complete(impute, 1)
colSums(is.na(newDATA))

# Define time variable
# Also, define the variables year, month, date, week of month 

time <- as.numeric(as.POSIXct(as.Date(as.character(newDATA$date),
                                      format="%m/%d/%Y"),origin = "1970-01-01"))

newDATA <- newDATA %>% separate(date,
                    c("month", "day","year"),
                    "/")
data_no_wip <- data_no_wip %>% separate(date,
                                c("month", "day","year"),
                                "/")

newDATA <- transform(newDATA, month = as.numeric(month), 
          day = as.numeric(day),
          year = as.numeric(year))
data_no_wip <- transform(data_no_wip, month = as.numeric(month), 
                     day = as.numeric(day),
                     year = as.numeric(year))


newDATA$week <- ceiling(newDATA$day / 7)
newDATA$time <- time

data_no_wip$week <- ceiling(data_no_wip$day / 7)
data_no_wip$time <- time


## Plot data: actual_productivity and targeted_productivity
# plot it by team or something !
plot(newDATA$time, newDATA$actual_productivity)
plot(newDATA$time, newDATA$targeted_productivity)