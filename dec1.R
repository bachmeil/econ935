library(tstools)
dgdp.raw <- read.csv("dgdp.csv", header=TRUE)
dgdp.all <- ts(dgdp.raw[,2], 
               start=c(1947,2),
               frequency=4)
dgdp <- window(dgdp.all, end=c(2019,4))

dummy <- dgdp < 0
dummy
neg <- lags(dummy,1)*lags(dgdp,1)
plot(neg)
rhs <- ts.combine(lags(dgdp,1),
                  dummy,
                  neg)
fit.threshold <- tsreg(dgdp, rhs)
fit.threshold
testzero(fit.threshold, c(3,4))
testzero(fit.threshold, 3)
testzero(fit.threshold, 4)
last(dgdp)
# forecast h=1
0.738 + 0.2964*0.4691

res <- fit.threshold$resids
mean(res)
plot(density(res))

set.seed(100)
e <- sample(res, size=1)
e

set.seed(100)
fcst.h2 <- replicate(1000, {
  e <- sample(res, size=1)
  dgdp.h1 <- 0.877 + e
  dgdp.h2 <- if (dgdp.h1 > 0) {
    0.738 + 0.2964*dgdp.h1
  } else {
    0.738 + 0.2964*dgdp.h1 -
      1.6965 - 0.4484*dgdp.h1
  }
  dgdp.h2
}, simplify="array")
fcst.h2
plot(density(fcst.h2))
mean(fcst.h2 > 0.5)
mean( (fcst.h2 > 0.5) & (fcst.h2 < 1.0) )
mean(fcst.h2)

set.seed(100)
fcst.h1 <- replicate(1000, {
  e <- sample(res, size=1)
  0.1 + e
}, simplify="array")
mean(fcst.h1 > 0)

set.seed(100)
fcst.h2 <- replicate(1000, {
  e <- sample(res, size=1)
  dgdp.h1 <- 0.1 + e
  dgdp.h2 <- if (dgdp.h1 > 0) {
    0.738 + 0.2964*dgdp.h1
  } else {
    0.738 + 0.2964*dgdp.h1 -
      1.6965 - 0.4484*dgdp.h1
  }
  dgdp.h2
}, simplify="array")
mean(fcst.h2)
0.738 + 0.2964*0.1
plot(density(fcst.h2))

theta <- function(gamma, dbar, y1){
  den <- 1 + exp(-gamma * (y1 - dbar))
  return(1/den)
}
theta(1, 0, 0.4)
theta(2, 0, 0.4)
theta(10, 0, 0.4)
theta(1000, 0, 0.4)
theta(1, 0, -0.4)
theta(2, 0, -0.4)
theta(10, 0, -0.4)
theta(1000, 0, -0.4)
plot(density(dgdp))

theta <- function(gamma, dbar){
  den <- 1 + exp(-gamma * (lags(dgdp,1) - dbar))
  return(1/den)
}
theta(1, 0)

sse <- function(gamma, dbar) {
  den <- 1 + exp(-gamma * (lags(dgdp,1) - dbar))
  theta <- 1/den
  rhs <- ts.combine(lags(dgdp,1),
                    theta,
                    theta*lags(dgdp,1))
  fit <- tsreg(dgdp, rhs)
  return(sum(fit$resids^2))
}
sse(1, 0)
sse(2.5, -0.2)
gamma.try <- c(0.1, 1, 5, 25, 1000)
dgdp.try <- sort(dgdp)[50:241]
plot(density(dgdp.try))
grid <- expand.grid(gamma.try, dgdp.try)
sse.values <- Map(sse, grid[,1], grid[,2])
sse.vector <- unlist(sse.values)
sse.vector
min(sse.vector)
which.min(sse.vector)
grid[195,]
th <- theta(1000, 0.45)
plot(density(th))
