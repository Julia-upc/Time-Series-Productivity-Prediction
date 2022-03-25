## Preprocessing 
library(mice)
library(dplyr)
library(tidyr)
library(lmtest)
library(caret)
library(TSstudio)

setwd("~/Escritorio/Time Series Data Mining/Productivity-Prediction-of-Garment-Employees-Data-Set")

# https://cran.r-project.org/web/packages/TSstudio/readme/README.html
# https://www.r-bloggers.com/2021/04/handling-missing-values-in-r/  

########## Preprocessing ARIMA ###############

# Read data
data<- read.csv("data/garments_worker_productivity.csv")

# Preprocessing
data <-
  transform(
    data,
    quarter = as.numeric( quarter )
  )

# Data Imputation methods 
impute <- mice(data, seed = 123) # estimates NA values 5 times by default
# pmm method: predictive Mean Matching
data <- complete(impute, 1)
#colSums(is.na(data))

data<- data %>% 
  mutate(department = ifelse(as.character(department) == "finishing ",
                             "finishing", as.character(department)))
data$D_T <- paste(data$department, data$team, sep = "")
data <- data[-c(3,5)]
# TS plot of each of the department teams 
ggplot(data,                            
       aes(x = as.numeric(date),
           y = actual_productivity,
           col = D_T)) + geom_line()

dmy <- dummyVars(" ~ .", data = data[3])
date<- as.Date(as.character(data$date),
               format="%m/%d/%Y")
data2 <- data.frame(predict(dmy, newdata = data))
data2$date <- date
data$date <- date

data$index <- c(1:dim(data)[1])
data2$index <- c(1:dim(data2)[1])
df = merge(x = data, y = data2, by = "index")
df<-df[-c(1,2,4)]

save(df, file ="data_arima.Rda")

########## Preprocessing CNN ###############
df <- df %>% separate(date.y,
                      c("year", "month","day"),
                      "-")
df <- transform(df,  month = as.numeric(month), 
                day = as.numeric(day),
                year = as.numeric(year))

dmy <- dummyVars(" ~ .", data = df)
df_nn <- data.frame(predict(dmy, newdata = df))

save(df_nn,file="data_cnn.Rda")