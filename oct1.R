regime1 <- function(y) {
  1.0 + 0.3*last(y) + rnorm(1)
}

regime2 <- function(y) {
  5.0 + 0.3*last(y) + rnorm(1)
}

set.seed(200)
coef <- replicate(400, {
  ysim1 <- tsgen(100, regime1, 0.0)
  ysim2 <- tsgen(150, regime2, last(ysim1))
  ysim <- ts(c(ysim1, ysim2))
  fit <- tsreg(ysim, lags(ysim,1))
  coefficients(fit)[2]
})
plot(density(coef))

ysim1 <- tsgen(1000, regime1, 0.0)
ysim2 <- tsgen(1500, regime2, last(ysim1))
ysim <- ts(c(ysim1, ysim2))
plot(ysim)

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




