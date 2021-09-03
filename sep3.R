library(tstools)
dbacon.raw <- read.csv("dbacon.csv", 
                       header=TRUE)
dbacon <- ts(dbacon.raw[,2], start=c(1980,2), 
             frequency=12)
ar2 <- arima(dbacon, order=c(2,0,0))
ar2
coef(ar2)
class(ar2)
predict(ar2, 2)
last(dbacon, 2)
0.3467 + 0.3111*2.80 - 0.0075*5.11
0.3467 + 0.3111*(2.80-0.3467) - 
  0.0075*(5.11-0.3467)
fit <- tsreg(dbacon, lags(dbacon,1:2))
fit
0.2567 + 0.3111*2.80 - 0.0075*5.11
0.2567 + 0.3111*1.089 - 0.0075*2.80
arma22 <- arima(dbacon, order=c(2,0,2))
arma22
ARMAacf(ar=0.9, lag.max=12)
ARMAacf(ar=c(0.9,0,0,0.02), lag.max=12)
ARMAacf(ar=0.9, ma=0.2, lag.max=12)
ARMAacf(ar=c(0.4, 0.2), ma=c(0.1, 0.7), 
        lag.max=12)
acf1 <- ARMAacf(ar=0.9, lag.max=12)
acf1
acf2 <- ARMAacf(ar=0.5, lag.max=12)
acf2
plot(acf1, ylim=c(0,1))
plot(acf2, ylim=c(0,1))
plot(ARMAacf(ar=c(0.1,0,0.8), lag.max=120))
ARMAacf(ar=coef(ar2)[1:2], lag.max=12)

ma.rep <- ARMAtoMA(ar=c(0.5, 0.3, 0.1), 
                   lag.max=60)
ma.rep
# MA(60) model
ma.acf <- ARMAacf(ma=ma.rep, lag.max=100)
acf2AR(ma.acf)[1:10,]
acf2AR(ma.acf)[3,]

maur.acf <- ARMAacf(ma=-1, lag.max=1000)
acf2AR(maur.acf)[1000,]
maur.acf <- ARMAacf(ma=-0.99, lag.max=1000)
acf2AR(maur.acf)[1000,]








