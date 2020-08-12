---
title: "Time_Series_Template_in_R"
author: "Sharath"
date: "07/08/2020"
output: html_document
editor_options:
chunk_output_type: console
---
library(kernlab)
library(kknn)
library(caret)
library(ggplot2)
library(dplyr)
library(lubridate)
library(MLmetrics)
library(forecast)
library(GeneCycle)
###################################################################################
#Setting the seed to retain the randomness and reading the temps file
###################################################################################
set.seed(1245)
data = read.delim("303_Train.csv", sep = ",")
###################################################################################
#Convert the data into timeseries
###################################################################################
#Train from 2017 January to 2019 November
data.full = ts(data,start=c(2017,1),end=c(2019,365),frequency=365)
cat(data.full)

data.ts = ts(data,start=c(2017,1),end=c(2019,334),frequency=365)
cat(data.ts)
#Test on 2019 December
data.test = window(data.full,start=c(2019,335),end=c(2019,365),frequency=365)
cat(data.test)
###################################################################################
#Preliminary data analysis
###################################################################################
autoplot(data.ts) +
  ggtitle("Flight bookings - Actuals from 2017 to 2019") +
  ylab("Booking counts per day")
###################################################################################
#Difference of data
###################################################################################
diff.ts = diff(data.ts)
###################################################################################
#Preliminary analysis of differenced data
###################################################################################
autoplot(diff.ts) +
  ggtitle("Flight bookings - Actuals from 2017 to 2019") +
  ylab("Booking counts per day")
###################################################################################
#Seasonality analysis
###################################################################################
ggseasonplot(diff.ts) +
  ggtitle("Seasonal plot - Flight bookings") +
  ylab("Booking counts per day")
###################################################################################
## fit the data to ES
###################################################################################
es_fit <- ets(data.ts)
print(summary(es_fit))
checkresiduals(es_fit) #20.19
es_fit$fitted
###################################################################################
# ES accuracy metrics
###################################################################################
mse <- RMSE(es_fit$fitted, data.ts)
cat("RMSE for ES model is found to be : ", mse)
mpe <- MAPE(es_fit$fitted, data.ts)
cat("MAPE for ES model is found to be : ", mpe)
###################################################################################
#Exponential Forecast for next 30 days
###################################################################################
es.fcst <- forecast(es_fit, h=31)
###################################################################################
#ES Testing RMSE and MAPE
###################################################################################
mse <- RMSE(es.fcst$mean, data.test)
cat("RMSE for ES model is found to be : ", mse)
mpe <- MAPE(es.fcst$mean, data.test)
cat("MAPE for ES model is found to be : ", round(mpe*100, 2))
###################################################################################
#Plot the forecase and compare with actuals
###################################################################################
autoplot(es.fcst,PI = FALSE) +
  autolayer(data.test) +
  scale_x_continuous(limits = c(2019.6,2020.1)) +
  autolayer(es.fcst$mean, series="Forecasts")
#autolayer(es.fcst$mean, series="Forecasts")
###################################################################################
#Fit Arima model
###################################################################################
ar_fit <- auto.arima(data.ts, d=1, D=1, stepwise=FALSE, approximation = FALSE, trace = TRUE)
#ar_fit <- auto.arima(data.ts)
print(summary(ar_fit))
checkresiduals(ar_fit) #20.19
###################################################################################
#Arima Training RMSE and MAPE
###################################################################################
mse <- RMSE(ar_fit$fitted, data.ts)
cat("RMSE for ARIMA model is found to be : ", mse)
mpe <- MAPE(ar_fit$fitted, data.ts)
cat("MAPE for ARIMA model is found to be : ", round(mpe*100, 2))
###################################################################################
#Arima Forecast for next 30 days
###################################################################################
ar.fcst <- forecast(ar_fit, h=31)
###################################################################################
#Arima Testing RMSE and MAPE
###################################################################################
mse <- RMSE(ar.fcst$mean, data.test)
cat("RMSE for ARIMA model is found to be : ", mse)
mpe <- MAPE(ar.fcst$mean, data.test)
cat("MAPE for ARIMA model is found to be : ", round(mpe*100, 2))
###################################################################################
#Write the Arima forecasts into a csv file
###################################################################################
write.csv(ar.fcst$mean, "303_Arima_actual_forecast.csv")
###################################################################################
#Plot the predicted vs actuals
###################################################################################
autoplot(ar.fcst,PI = FALSE) +
  autolayer(data.test) +
  scale_x_continuous(limits = c(2019.6,2020.1)) +
  autolayer(es.fcst$mean, series="Forecasts")
#coord_cartesian(xlim = c(2019.6, 2020.1), ylim = c(0, 150))
#coord_cartesian(xlim = c(2019.6, 2020.1), ylim = c(0, 150))
#autolayer(es.fcst$mean, series="Forecasts")
