library(tstools)
u.raw <- read.csv("unrate.csv", header=TRUE)
u.raw
u.all <- ts(u.raw[,2], start=c(1948,1),
        frequency=12)
u <- window(u.all, end=c(2019,12))
plot(u)

inf.raw <- read.csv("https://raw.githubusercontent.com/bachmeil/econ935/main/inflation.csv")
inf.raw
inf.all <- ts(inf.raw[,2], start=c(1947,2),
            frequency=12)
inf <- window(inf.all, end=c(2019,12))
plot(inf.all)
rhs <- ts.combine(lags(u, 1:2), lags(inf, 1:2))
fit.u <- tsreg(u, rhs)
fit.u
fit.inf <- tsreg(inf, rhs)
fit.inf

rhs4 <- ts.combine(lags(u, 4:5), 
                   lags(inf, 4:5))
fit.inf4 <- tsreg(inf, rhs4)
fit.inf4
last(u, 2)
last(inf, 2)
0.10 - 0.06*3.6 + 0.07*3.6 + 0.22*0.08 + 0.24*0.23
tsobs(inf.all, c(2020,4))

library(vars)
phillips <- ts.combine(u, inf)
varfit <- VAR(phillips, lag.max=12, ic="SC")
varfit <- VAR(phillips, lag.max=12, ic="AIC")
varfit
VARselect(phillips, lag.max=13)$selection["AIC(n)"]
pred <- predict(varfit, n.ahead=1)
pred
pred$fcst
pred$fcst$u
pred$fcst$u[,"fcst"]
getVarForecast(varfit, 2, n=1)
getVarForecast(varfit, "u", n=1)
getVarForecast(varfit, "u", n=12)
getVarForecasts(varfit, "u", n=1:12)
getVarForecasts(varfit, "u", n=1:12,
                start=c(2020,1))
plot(getVarForecasts(varfit, "u", n=1:12,
                start=c(2020,1)))





