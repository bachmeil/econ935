library(tstools)
income.raw <- read.csv("disposable.csv",
                       header=TRUE)
income <- ts(income.raw[,2], start=c(1959,1),
             frequency=4)
plot(income)
income19 <- window(income, end=c(2019,4))
plot(income19)

pce.raw <- read.csv("pce.csv",
                       header=TRUE)
pce <- ts(pce.raw[,2], start=c(1959,1),
             frequency=4)
plot(pce)
pce19 <- window(pce, end=c(2019,4))
plot(pce19)

dincome <- pctChange(income19)
dpce <- pctChange(pce19)
plot(dincome)
plot(dpce)
plot(diff(pce19))
plot(log(pce19))

# Reduced form VAR
rhs <- ts.combine(lags(dincome, 1:3),
                  lags(dpce, 1:3))
fit.income <- tsreg(dincome, rhs)
fit.income
fit.pce <- tsreg(dpce, rhs)
fit.pce

# Estimate CI vector
fit.eg <- tsreg(pce19, income19)
fit.eg
lpce19 <- log(pce19)
lincome19 <- log(income19)
fit.eg <- tsreg(lpce19, lincome19)
fit.eg
fit.eg2 <- tsreg(income19, pce19)
fit.eg2
fit.eg3 <- tsreg(income19, pce19, intercept=FALSE)
fit.eg3
z <- fit.eg$resids
plot(z)

# Vector error correction
rhs.ecm <- ts.combine(lags(dincome, 1:3),
                  lags(dpce, 1:3), lags(z,1))
ecm.pce <- tsreg(dpce, rhs.ecm)
ecm.pce
ecm.income <- tsreg(dincome, rhs.ecm)
ecm.income








