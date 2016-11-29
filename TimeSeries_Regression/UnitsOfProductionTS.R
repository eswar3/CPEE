rm(list=ls(all=TRUE))
setwd("G:/Data Science/GNQ_Preperation_2/Test/20150823_GNQ_Datasets")
setwd("F:/AA_Clases/20160417/201601017_LabDay7202/Time_Series")
MyDataProduction <- read.csv("DayWiseUnitsProduction.csv",header = T)
a = (as.Date(MyDataProduction$Date,format='%d/%m/%Y'))
b = as.Date(MyDataProduction$Date,format='%d-%m-%Y')
table(is.na(a))
table(is.na(b))	
a[is.na(a)] <- b[!(is.na(b))]
table(is.na(a))
a
# install.packages("xts")
library(xts)
MyDataProduction$Date <- a
# MyDataProduction <- MyDataProduction[,-1]
# MyDataProduction <- cbind(MyDataProduction,a)

# install.packages("xts")
# library(xts)

AggregatedWeek <-apply.weekly(xts(MyDataProduction$No..of.Units..in.thousands.,order.by = MyDataProduction$Date),FUN = mean)
AggregatedWeekTrain <- AggregatedWeek[1:96]
head(AggregatedWeekTrain)
is.ts(AggregatedWeekTrain)
TSDt <- ts(AggregatedWeekTrain,frequency = 4)
head(TSDt)
## aggregated by week


timeseries_decomposed <- decompose(TSDt)
plot(timeseries_decomposed)

###
# install.packages("TTR")
library(TTR)
smaProd = SMA(TSDt, n=4)
wmaProd = WMA(TSDt, n=4)
emaProd = EMA(TSDt, n=4)

plot(TSDt)
lines(smaProd,col="blue")
lines(wmaProd,col="green")
lines(emaProd,col="red")

#HoltWinter's
library(forecast)
prodForecast <- HoltWinters(TSDt)
plot(prodForecast)
prodForecast$SSE


#SSE reducing when radomness component is removed from Model
prodForecastWithoutRandomness <- HoltWinters(TSDt,gamma = FALSE)
plot(prodForecastWithoutRandomness)
prodForecastWithoutRandomness$SSE

#with trend, randomness and seasonality
prodForecast
prodForecast$fitted
prodForecast_holt <- forecast.HoltWinters(prodForecast,h=12)
plot(prodForecast_holt)

### Arima

ARIMADST <- auto.arima(TSDt)
arimaForecast <- forecast.Arima(ARIMADST,h=12)
### arima(1,0,1)
plot(arimaForecast)

ARIMADST$residuals

plot(TSDt)