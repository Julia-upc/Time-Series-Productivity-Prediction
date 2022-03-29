library(caret)
library(dplyr)
library(TSstudio)
library(mice)
library(forecast)

setwd("~/Escritorio/Time Series Data Mining/Productivity-Prediction-of-Garment-Employees-Data-Set")
load(file="data/data_arima.Rda")

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
        Ytitle = "Actual productivity",
        Xtitle = "Time")

ts_plot(ts2, 
        title = "Sewing department team 1",
        Ytitle = "Actual productivity",
        Xtitle = "Time")
Acf(ts2,  title = "Finishing department team 1")
Pacf(ts2,  title = "Finishing department team 1")

########################## IMPLEMENTING ARIMA #################################

# Buscar primero parametros p,d,q con autoarima. Luego hacer fit y coeftest 
# para ver que variables del dataset son significativas. Finalmente haces un 
# modelo final fit con las variables que son buenas y haces la pred. 
# Para mirar lo bueno que es el modelo miras el bic y el aic.
# Finalmente calcular MSE de las predicciones. 


# Setting training and testing partitions

df1_s <- ts_split(ts.obj = as.ts(df1), sample.out = 10)
train_1 <- df1_s$train
test_1 <- df1_s$test

df2_s <- ts_split(ts.obj = as.ts(df2), sample.out = 10)
train_2 <- df2_s$train
test_2 <- df2_s$test

# TS_2

md <- auto.arima(train_2[,11], xreg=train_2[,-c(6,7,8,9,11,12,13,14,15,16,17)])
coeftest(md)
md <- auto.arima(train_2[,11], xreg=train_2[,c(2,3,10)])
coeftest(md)

fc <- forecast(md, xreg=train_2[,c(2,3,10)], h = 12)
# sewing
summary(fc)

plot(fc)
lines(test_2[,11],col="red")
lines(fc$fitted, col= "chartreuse3")
# analysis of the residuals
plot(fc$residuals, xlab = "Time",ylab ="Residuals" )
hist(fc$residuals, ylab = "Frequency",xlab ="Residuals")
Acf(md$residuals) 
Pacf(md$residuals)

plot(lm(fc$residuals~fc$fitted))

# TS_1
md <- auto.arima(train_1[,11], xreg=train_1[,c(2,3,10)])
coeftest(md)
fc <- forecast(md, xreg=train_1[,c(2,3,10)], h = 12)
# finishing
summary(fc)

plot(fc)
lines(test_1[,11],col="red")
lines(fc$fitted, col= "chartreuse3")
# analysis of the residuals
plot(fc$residuals, xlab = "Time",ylab ="Residuals" )
hist(fc$residuals, ylab = "Frequency",xlab ="Residuals")
Acf(md$residuals) 
Pacf(md$residuals)
plot(lm(fc$residuals~fc$fitted))
