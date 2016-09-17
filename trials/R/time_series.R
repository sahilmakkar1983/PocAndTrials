# Time series


# Component of time-series
# 1. Long term trend
# 2. Seasonal variation : Example - Increase of sale due to winter
# 3. Cyclical variation : Example - increase of sale due to inflation
# 4. Irregular variation
#     Anything not explained by 1-3 is called irregular variation, it is of two types:
#     a) Stationary variation : When data neither increases nor decreases
#     b) UnStationary variation: When data has some explainable portion remaining and can be analysed further.

# Decomposition of time-series:
# = trend-compo + irregular-compo (+seasonal compo if seasonal time-series)
# Addictive decomposition model: Y = Trend + seasonal + regular
# Multipicative decomposition model: Y = Trend*seasonal*regular
# De-seasonalized time-series: if seasonal component is removed from time-series

# R function = decompose() - this  is for addictive model
# decompose function estimates seasonal, trend and irregular component and it is stored in "seasonal", "trend" and "random".
# Decompose function uses Moving-avrage for estimation of trend compo
# Decompose function uses averagingg-observations for estimation of seasonal compo.
# Decompose function calculates error compo by substracting the earlier values from time-series

# Moving average(applied on Trend component; i.e on non-seasonal TS), MA types:
# SMA - simple moving average
# EMA - exponential moving average
# WMA - Weighted moving average

# Exponential smoothning: (using log function)
# Used on TS which can be described by addictive model, with no seasonality 
# It can make short-term forecasts
# F(t+1) = alpha*Y(t) + (1-alpha)*F(t) - forecast equation
#   Value of alpha can be choosen using MSE (mean square error) method. We should do hit and trial to find best alpha value.
#   error = Y(t+1) - F(t+1); -> error^2 ->sum of error^2 = SSE -> SSE/N = MSE

# Holt's exponential smoothning
# It considers trend value in TS data
# it fits trend value non-parametrically
# Smoothning done in two ways:
#  1. First, like in ES - smooth b/w last actual data and it's forcasted data (F(t+1) = alpha*Y(t) + (1-alpha)*F(t))
#  2. Secondly, we find out trend-compo by smoothning data b/w the last two forecasted values and last trend calculated.
#    T(t+1) = y*(F(t+1) - F(t)) + (1-y)*T(t)
# H(t+1) = F(t+1) + T(t+1)


# HOlt's winter Exponential smootning
# It considers SEASONAL-compo as well
# It assumes multiplicative seasonal effect and addictive trend component.

# TBD
# White noise - short note : If it is present - we won't get any forecast from TS
# ETS is used for non-stationary 
# ACF
# Box-jenkins
#  Applies on stationary TS
#  Its nothing but ARMA
#  It converts non-stationary to stationary using ARIMA
# ARMA
# ARIMA

library("foreign", lib.loc="C:/Program Files/R/R-3.2.2/library")

# Exploratory analysis - youtube tutorials
data("AirPassengers")
AP<-AirPassengers
# class function is used to get type(factor, numeric etc.) of data
class(AP)
start(AP)
end(AP)
frequency(AP)
?frequency
summary(AP)
plot(AP)
cycle(AP)
?cycle
aggregate(AP)
?aggregate
aggregate(AP, FUN=mean)
plot(aggregate(AP))
boxplot(AP~cycle(AP))
plot(AP)

# My own trials
par(mfrow=c(1,1))
monthplot(AP,cycle(AP), base=mean)
a <- decompose(AP)
a
names(a)
boxplot(a$seasonal, a$trend, a$random)
plot(a$trend)
plot(a$seasonal)
plot(a$random)

#Plotting time-series and plotting a trend line on top of it
#START
plot(AP)
lines(a$trend, col="red")
#END

#MA trials
plot(AP, col="gray")
lines(ma(AP, 5), col="red")
?ma
??seasadj
#END


plot(a - a$trend)
acf(AP)
pacf(AP)
acf(AP, plot=FALSE)
pacf(AP, plot=FALSE)
names(pacf(AP))
?pacf

b = stl(AP, s.window=12)
names(b)
head(b)

# Conclusion: 
# 1. There is continual growth over years.
# 2. There is seasonal peak during July and August month
# 3. There is seasonal low in November.



# Data 2 - like we get acutally; with little of data treatment
webdata <- "http://rci.rutgers.edu/~rwomack/UNRATE.csv"
webdata2 <- "http://rci.rutgers.edu/~rwomack/CPIAUCSL.csv"
unemployment <- read.csv(webdata, row.names = 1)
inflation <- read.csv(webdata2, row.names = 1)
?read.csv
summary(inflation)
head(unemployment)
head(inflation)
str(inflation)
class(inflation)
class(unemployment)

# Convert data to timeseries
Irate <- ts(inflation$VALUE, start=c(1948,1), freq=12)
Urate <- ts(unemployment$VALUE, start=c(1948,1), freq=12)
plot(Irate)
plot(Urate)

Urate.june <- window(Urate, start=c(1948,6), freq=T)
plot(Urate.june)
time(Urate.june)
plot(Urate)
abline(reg=lm(Urate~time(Urate)))
plot(Urate, Irate, col=c("blue", "red"))
ts.plot(Urate, Irate, col=c("blue", "red"))

# Analyze time-series
acf(Urate)
pacf(Urate)
acf(AP)
pacf(AP)
acf(ts.intersect(Urate,AP))

#Holt Winter
plot(HoltWinters(Urate, alpha = .5, beta = .1, gamma =.05)) 
#Alpha is about smoothing LEVEL component
#Beta is about smoothing TREND component
#Gamma is about smoothing SEASONAL component

#AP with Holt Winter
#=====================
plot(HoltWinters(AP, alpha = .1, beta = .2, gamma =.8)) 
#Analyze the graphs properly, try different values of alpha, beta and gamma by hit-and-trial method

#Model it now
# If you don't pass alpha, beta, gamma, R would automatically find proper values
AP.hw<-HoltWinters(AP)
#Predict now
AP.predict<-predict(AP.hw,n.ahead=5*12)
ts.plot(AP, AP.predict, lty=1:2, col=c("blue", "red"))

acf(Urate)
pacf(Urate)
Urate.hw<-HoltWinters(Urate)
#Predict now
Urate.predict<-predict(Urate.hw,n.ahead=5*12)
plot(decompose(Urate))
plot(decompose(AP))
ts.plot(Urate, Urate.predict, lty=1:2, col=c("blue", "red"))

#Model on Urate looks sluggish and crappy, try ARIMA(1,1,1)
Urate.arima<-arima(Urate, order=c(2,2,2))
#Predict now
Urate.predict<-predict(Urate.arima,n.ahead=10*12)
ts.plot(Urate, Urate.predict$pred, lty=1:2, col=c("blue", "red"))

#randomwalk
x<-w<-rnorm(1000)
for (t in 2:1000) x[t]<-x[t-1]+w[t]
plot(x, type="l")
acf(x)
acf(diff(x))
pacf(x)

#Stationary AR process
x<-w<-rnorm(1000)
for (t in 2:1000) x[t]<-(x[t-1]/2)+w[t]
plot(x, type="l")
acf(x)
acf(diff(x))
pacf(x)

#AR model
Urate.ar<-arima(Urate, order=c(1,0,0))
x.ar<-arima(x, order=c(1,0,0))
coef(Urate.ar)
names(Urate.ar)
head(Urate.ar)
?arima



#Beyond video lecture parts
library(fpp)
library(forecast)
acf(Urate)
pacf(Urate)
acf(AP)
pacf(AP)
Box.test(Urate, lag=20, type="Ljung-Box")
Box.test(AP, lag=20, type="Ljung-Box")
#Result reading: Low p-value indicates both are stationary series
adf.test(Urate, alternative="stationary")
adf.test(AP, alternative="stationary")
#Result reading: Low p-value indicates both are stationary series
kpss.test(Urate)
kpss.test(AP)
#Result reading: Low p-value indicates both are stationary series
