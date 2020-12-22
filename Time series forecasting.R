install.packages("cluster")
library(cluster)
library(tidyverse)
library(factoextra)
library(tseries)
library(forecast)

######regression analysis

#loading the csv to R
Airport <- read.csv(choose.files())
#plotting scatter plot of the data for domestic and international passengers
plot(Airport$Year, Airport$Domestic.Passengers, xlab = "Year", ylab = "Domestic passengers", col="red")
plot(Airport$Year, Airport$International.Passengers, xlab = "Year", ylab = "International Passengers", col="red")

#boxplot for international and domestic passengers
boxplot(International.Passengers~ Year, data = Airport, xlab="year", ylab= "International passenger", col="red") 
boxplot(Domestic.Passengers~ Year, data = Airport, xlab= "Year", ylab= "Domestic Passenger", col="red")



#######forecasting domestic passengers for the year 2016

#creating time series object for domestic passngers
domesticts <- ts(Airport$Domestic.Passengers)
domesticts
#assigning the cycle and year
domesticts<- ts(domesticts, frequency = 12, start = c(1977,1))
domesticts
plot.ts(domesticts)

#decomposing the time series data into trend, season and random
domesticdecompose <- decompose(domesticts)
#plotting the decomposed data
plot(domesticdecompose)
#adjusting seasonality
domesticseasonalajuts <- domesticts - domesticdecompose$seasonal
plot.ts(domesticseasonalajuts)
#checking the stationarity 
adf.test(domesticseasonalajuts)

#getting the ARIMA parameters (p,d,q)
domesticarima <- auto.arima(domesticseasonalajuts)
domesticarima
plot.ts(domesticarima$residuals)

# forecasting the ARIMA 
domesticforecast <- forecast(domesticarima, h=12)
domesticforecast
Box.test(domesticforecast, lag = 20, type= "Ljung-Box")



#forecasting international passengers for the year 2016

#loading the internatioanl data into time series
internationalts <- ts(Airport$International.Passengers)
internationalts
#assigning the cycle amd year to the time series
internationalts <- ts(internationalts, frequency = 12, start= c(1977,1))
internationalts
#plotting the time series data
plot.ts(internationalts)
#decomposing the time series data into trend, season and random
internationadecompose <- decompose(internationalts)
plot(internationadecompose)
#adjusting the seasnality
internationalseasonaladjust <- internationalts - internationadecompose$seasonal
plot.ts(internationalseasonaladjust)
#checking the stationarity of the data
adf.test(internationalseasonaladjust)

#getting the ARIMA parameters (p,d,q)
internationalarima <- auto.arima(internationalseasonaladjust)
internationalarima
plot.ts(internationalarima$residuals)

#forecasting the arima model
internationalforecast <- forecast(internationalarima, h=12)
internationalforecast
#checking the model
Box.test(internationalforecast, lag = 20, type = "Ljung-Box")
