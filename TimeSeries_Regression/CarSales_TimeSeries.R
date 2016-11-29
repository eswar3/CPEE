rm(list = ls(all.names = TRUE))
# setwd("F:/AA_Clases/20160417")
setwd("F:/AA_Clases/20160626_TimeSeries_Batch17/Data&Rcode")
# setwd("F:/AA_Clases/20160626_TimeSeries_Batch17/Time_Series")
Data <- read.csv("monthly-car-sales-in-quebec-1960.csv",header = T)

TrainData <- Data[1:84,]

library(tseries)
TSData <- ts(TrainData,start = c(1960,1),frequency = 12)
# TSData[]
# Plotting TS
plot.ts(TSData[,2])
# Decomposing TS
DecomposeTimeSeries <- decompose(TSData[,2])
plot(DecomposeTimeSeries)

library(forecast)
#Holt Winters
# Creating Holts Model
HoltWinterModel1 <- HoltWinters(TSData[,2],alpha = TRUE,beta = FALSE,gamma = TRUE)
HoltWinterModel1$fitted
plot(HoltWinterModel1)
# Forecasting Holt's Model

HoltPrediction <- forecast(HoltWinterModel1,h=24)
Data$Monthly.car.sales.in.Quebec.1960.1968[86:nrow(Data)-1]
#Calculating the Error metric for Holts model
mean(abs(data.frame(HoltPrediction)$Point.Forecast-
Data$Monthly.car.sales.in.Quebec.1960.1968[86:nrow(Data)-1]))
# 1770.458
par(mfrow=c(1,2))
# To Forecast using ARIMA arriving at p,d,q value using the ACF,PACF plots
acf(TSData[,2])
pacf(TSData[,2])

FirstOrderTS <- diff(TSData[,2],differences = 1)
SecondOrderTS <- diff(TSData[,2],differences = 2)
ThirdOrderTS <- diff(TSData[,2],differences = 3)

acf(FirstOrderTS)
pacf(FirstOrderTS)

acf(SecondOrderTS)
pacf(SecondOrderTS)

acf(ThirdOrderTS)
pacf(ThirdOrderTS)
plot.ts(FirstOrderTS)
plot.ts(SecondOrderTS)
plot.ts(ThirdOrderTS)
# d=3,p=7,q=5
# Running the model ARIMA
ArimaFirstOrder <- arima(FirstOrderTS,order = c(1,1,1))
# Forecasting using ARIMA
ArimaForecast <- forecast.Arima(ArimaFirstOrder,h=24)
# Calculating the error metrics using 
mean(abs(data.frame(ArimaForecast)$Point.Forecast - Data$Monthly.car.sales.in.Quebec.1960.1968[86:nrow(Data)-1]))
