library(tstools)
u.raw <- read.csv("unrate.csv", header=TRUE)
u.all <- ts(u.raw[,2], start=c(1948,1),
            frequency=12)
u <- window(u.all, end=c(2019,12))
inf.raw <- read.csv("inflation.csv", header=TRUE)
inf.all <- ts(inf.raw[,2], start=c(1947,2),
              frequency=12)
inf <- window(inf.all, end=c(2019,12))
fit.linear <- tsreg(inf, lags(u,1))
fit.linear
dum <- lags(u,1) < 4.0
dumvar <- dum*lags(u,1)
dumvar
fit.threshold <- tsreg(inf, ts.combine(
  lags(u,1), dumvar))
fit.threshold
summary(fit.threshold)
library(sandwich)
v.nw <- NeweyWest(fit.threshold)
v.nw
se.nw <- sqrt(diag(v.nw))
t.nw <- coefficients(fit.threshold)/se.nw
t.nw
