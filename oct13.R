library(tstools)
library(vars)
u.raw <- read.csv("unrate.csv", header=TRUE)
u.raw
u.all <- ts(u.raw[,2], start=c(1948,1),
            frequency=12)
u <- window(u.all, end=c(2019,12))

inf.raw <- read.csv("https://raw.githubusercontent.com/bachmeil/econ935/main/inflation.csv")
inf.raw
inf.all <- ts(inf.raw[,2], start=c(1947,2),
              frequency=12)
inf <- window(inf.all, end=c(2019,12))

phillips <- ts.combine(u, inf)


varfcst <- function(t) {
  ds <- window(phillips, end=t)
  fit <- VAR(ds, lag.max=6, ic="SC")
  return(getVarForecast(fit, "inf", 1))
}
varfcst(c(2012,4))

dates <- make.dates(c(2009,12), c(2019,11),
                    12)
dates
varfcst(dates[1])
varfcst(dates[2])
varfcst(dates[3])

fcst.var <- lapply(dates, varfcst)
fcst.var
fcst.var <- unlist(lapply(dates, varfcst))
fcst.var
fcst.var <- ts(unlist(lapply(dates, varfcst)),
               start=c(2010,1), frequency=12)
fcst.var

varfcst <- function(t) {
  ds <- window(phillips, end=t)
  fit <- VAR(ds, p=4)
  return(getVarForecast(fit, "inf", 1))
}
fcst.var <- ts(unlist(lapply(dates, varfcst)),
               start=c(2010,1), frequency=12)

make.fcsts <- function(f, endDates, 
                       firstForecast) {
  return(ts(unlist(lapply(endDates, f)),
     start=firstForecast, 
     frequency=frequency(phillips)))
}

arfcst <- function(t) {
  ds <- window(phillips, end=t)
  fit <- arima(ds[, "inf"],
               order=c(4,0,0))
  pred <- predict(fit, 1)
  return(pred$pred)
}         
arfcst(c(2012,1))         
         
f.var <- make.fcsts(varfcst, dates, c(2010,1))         
f.ar <- make.fcsts(arfcst, dates, c(2010,1))         
actual <- window(inf, start=c(2010,1),
                 end=c(2019,12))
e.var <- actual - f.var
e.ar <- actual - f.ar  
e.var  
plot(e.ar)  
# Loss differential series
d <- e.var^2 - e.ar^2
summary(lm(d ~ 1))  

num <- sum(e.ar^2 - e.ar*e.var)
den <- sum(e.var^2)  
enc.new <- 120*num/den  
enc.new  
mean(e.var^2)  
mean(e.ar^2)  

varfcst <- function(t) {
  ds <- last(window(phillips, end=t), 120)
  fit <- VAR(ds, p=4)
  return(getVarForecast(fit, "inf", 1))
}
varfcst(c(2013,4))

tail(window(phillips, end=c(2013,4)), 120)  
last(window(phillips, end=c(2013,4)), 120)
  