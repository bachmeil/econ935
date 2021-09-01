dgdp.raw <- read.csv("dgdp.csv", header=TRUE)
dgdp <- ts(dgdp.raw[,2], start=c(1947,2), 
           frequency=4)
plot(dgdp)

dbacon.raw <- read.csv("dbacon.csv", 
                       header=TRUE)
dbacon <- ts(dbacon.raw[,2], start=c(1980,2), 
           frequency=12)
plot(dbacon)

gdp.ma1 <- arima(dgdp, order=c(0,0,1))
class(gdp.ma1)
names(gdp.ma1)
gdp.ma1$coef
gdp.ma1$residuals
AIC(gdp.ma1)
BIC(gdp.ma1)
predict(gdp.ma1,1)
library(tstools)
last(residuals(gdp.ma1))
0.768 + 0.1015*0.75
predict(gdp.ma1,12)

bacon.ma2 <- arima(dbacon, order=c(0,0,2))
predict(bacon.ma2, 3)
coef(bacon.ma2)
last(residuals(bacon.ma2),2)
0.3468 + 0.3136*1.193 + 0.128*4.096
0.3468 + 0.128*1.193

info.crit <- function(q) {
  fit <- arima(dgdp, order=c(0,0,q))
  return(c(aic=AIC(fit), sic=BIC(fit)))
}
info.crit(1)
info.crit(2)
info.crit(3)
gdp.crit <- lapply(1:12, info.crit)
gdp.crit
