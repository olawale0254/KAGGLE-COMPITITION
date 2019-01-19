##call data cfa
data1 <- read.csv(file.choose(),header = FALSE)

#### make it a time series data
data2 <- ts(data1, frequency = 12, start =c(2007,1))
data2
##call data Naira
data3 <- read.csv(file.choose(),header = FALSE)
#### make it a time series data
data4 <- ts(data3, frequency = 12, start =c(2007,1))
data4
#plot time series 
plot.ts(data2)
plot.ts(data4)
#detemine stationarity of data
#detemine stationarity of data
acf(data2, lag.max=34)
pacf(data2, lag.max=34)
tsstationary = diff(data2, differences=12)
plot.ts(tsstationary)
#detemine stationarity of data
acf(data4, lag.max=34)
pacf(data4, lag.max=34)
tsstationary1 = diff(data4, differences=18)
plot.ts(tsstationary1)
library(tseries)
library(timeSeries)
adf.test(tsstationary, alternative="stationary", k=0)###### cefa
adf.test(tsstationary1, alternative="stationary", k=0)###### Dollar
#acf/pacf
pacf(tsstationary, lag.max=50)
acf(tsstationary, lag.max=50)
#acf/pacf
acf(tsstationary1, lag.max=50)
pacf(tsstationary1, lag.max=50)
#fit the model
#fit the model
###fit the ARIMA MODEL
library(forecast)
library(ggplot2)
Arima1 <- auto.arima(tsstationary, trace=TRUE) 
Arima2 <- auto.arima(tsstationary1, trace = TRUE)
Arima1
Arima2
#forcast future values
library(forecast)
### forecast
par(mfrow = c(1,1))
cefaforecast <- forecast(Arima1, h=30)
cefaforecast
plot(cefaforecast)
Dollarforecast <- forecast(Arima2, h=30)
Dollarforecast
plot(Dollarforecast)
a <-predict(Arima2, n.ahead = 30)
plot(Arima1$fitted)

