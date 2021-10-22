dgdp.raw <- read.csv("dgdp.csv", header=TRUE)
dgdp <- ts(dgdp.raw[,2], start=c(1947,2), frequency=4)
plot(dgdp)
ffr.raw <- read.csv("fedfunds.csv", header=TRUE)
ffr <- ts(ffr.raw[,2], start=c(1954,3), frequency=4)
plot(ffr)
dffr <- diff(ffr)
plot(dffr)
library(tstools)
fit <- tsreg(dgdp, lags(dffr,0:12),
             start=c(1984,1), end=c(2019,4))
fit
cumsum(coefficients(fit)[-1])
acf(dffr)

fit.r <- tsreg(ffr, ts.combine(lags(dgdp, 0:1),
                               lags(ffr,1)))
mon.shock <- fit.r$resids
fit.y <- tsreg(dgdp, lags(mon.shock,0:12))
fit.y
irf <- cumsum(coefficients(fit.y)[-1])
plot(ts(irf))
