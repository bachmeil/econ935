library(tstools)
library(vars)
u.raw <- read.csv("unrate.csv", header=TRUE)
u.raw
u.all <- ts(u.raw[,2], start=c(1948,1),
            frequency=12)
u <- window(u.all, end=c(2019,12))

inf.raw <- read.csv("inflation.csv", header=TRUE)
inf.raw
inf.all <- ts(inf.raw[,2], start=c(1947,2),
              frequency=12)
inf <- window(inf.all, end=c(2019,12))

# Convert a vector of parameters
# into the deviation from the moment
# conditions
objfnc <- function(par) {
  a <- par[1]
  b <- par[2]
  res <- inf - a - b*lags(u,1)
  dev1 <- mean(res)
  dev2 <- mean(res*lags(u,1))
  return(1000*(dev1^2 + dev2^2))
}
objfnc(c(0.4, 0.3))  
optim(c(0.4, 0.3), objfnc,
      control=list(maxit=10))  
tsreg(inf, lags(u,1))  

# Now do the moment restrictions of the SVAR
objfnc2 <- function(par) {
  a0 <- par[1]
  a1 <- par[2]
  a2 <- par[3]
  b0 <- par[4]
  b1 <- par[5]
  b2 <- par[6]
  b <- par[7]
  c <- 0
  u.var <- par[8]
  inf.var <- par[9]
  res.u <- u - a0 - a1*lag(u,1) -
    a2*lag(inf,1)
  res.inf <- inf - b0 - b1*lag(u,1) -
    b2*lag(inf,1)
  rfvar.u <- mean(res.u^2)
  rfvar.inf <- mean(res.inf^2)
  rfcov <- mean(res.u*res.inf)
  dev1 <- rfvar.u - u.var - b^2*inf.var
  dev2 <- rfvar.inf - c^2*u.var -
    inf.var
  dev3 <- rfcov - c*u.var - b*inf.var
  return(dev1^2 + dev2^2 + dev3^2)
}
objfnc2(c(rep(0.0, 7), 0.1, 0.1))  
optim(c(rep(0.0, 7), 0.1, 0.1), objfnc2,
      control=list(maxit=10000))  
  
  
  
  
  
  
