library(caret)
library(dplyr)
library(TSstudio)
setwd("~/Escritorio/Time Series Data Mining/Productivity-Prediction-of-Garment-Employees-Data-Set")

# https://cran.r-project.org/web/packages/TSstudio/readme/README.html
# https://www.r-bloggers.com/2021/04/handling-missing-values-in-r/  

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

# Split the DF into 24 based on the variable D_T
X<-split(df, df$D_T)

df1 <- X$finishing1
df1$time <- c(1:dim(df1)[1])
df1 <- df1[-c(12)]
ts1 <-as.ts(df1$actual_productivity)

df2 <- X$sweing1
df2$time <- c(1:dim(df2)[1])
df2 <- df2[-c(12)]
ts2 <- as.ts(df2$actual_productivity)

ts_plot(ts1, 
        title = "Finishing department team 1",
        Ytitle = "Actual productivity")

ts_plot(ts2, 
        title = "Sweing department team 1",
        Ytitle = "Actual productivity")

########################## IMPLEMENTING ARIMA #################################

# Buscar primero parametros p,d,q con autoarima. Luego hacer fit y coeftest 
# para ver que variables del dataset son significativas. Finalmente haces un 
# modelo final fit con las variables que son buenas y haces la pred. 
# Para mirar lo bueno que es el modelo miras el bic y el aic.
# Finalmente calcular MSE de las predicciones. 


# Setting training and testing partitions

df1_s <- ts_split(ts.obj = as.ts(df1), sample.out = 5)
train_1 <- df1_s$train
test_1 <- df1_s$test

df2_s <- ts_split(ts.obj = as.ts(df2), sample.out = 5)
train_2 <- df2_s$train
test_2 <- df2_s$test

# TS_2
md <- auto.arima(train_2[,11], xreg=train_2[,-c(6,7,8,9,11,12,13,14,15,16,17)])
coeftest(md)
md <- auto.arima(train_2[,11], xreg=train_2[,c(2,3,10)])
coeftest(md)

fc <- forecast(md, xreg=train_2[,c(2,3,10)], h = 5)

summary(fc)

plot(fc)
lines(test_2[,11],col="red")
lines(fc$fitted, col= "blue")
# analysis of the residuals
plot(fc$residuals)
hist(fc$residuals)
Acf(md$residuals) 
Pacf(md$residuals)


# TS_1
md <- auto.arima(train_1[,11], xreg=train_1[,c(2)])
coeftest(md)
fc <- forecast(md, xreg=train_1[,c(2)], h = 5)

summary(fc)

plot(fc)
lines(test_1[,11],col="red")
lines(fc$fitted, col= "blue")
# analysis of the residuals
plot(fc$residuals)
hist(fc$residuals)
Acf(md$residuals) 
Pacf(md$residuals)


