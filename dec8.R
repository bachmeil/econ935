# Demonstrate the activation function
library(tstools)
dgdp.raw <- read.csv("dgdp.csv", header=TRUE)
dgdp <- ts(dgdp.raw[,2], start=c(1947,2), frequency=4)
ffr.raw <- read.csv("fedfunds.csv", header=TRUE)
ffr <- ts(ffr.raw[,2], start=c(1954,3), frequency=4)
core.raw <- read.csv("corecpi.csv", header=TRUE)
core <- ts(core.raw[,2], start=c(1957,1),
           frequency=4)
coreinf <- 400*pctChange(core)
plot(coreinf)

tsdata <- ts.combine(dgdp, coreinf, ffr)

g <- function(w) {
  w0 <- w[1]
  w1 <- w[2]
  w2 <- w[3]
  w3 <- w[4]
  step1 <- w0 + w1*dgdp + w2*ffr + w3*coreinf
  step2 <- step1 > 0
  return(step1 * step2)
}
plot(g(c(-2.4, 1, 1, 1)))
plot(g(c(-2.4, -1, -1, -1)))
plot(g(c(-2.4, 1, -1, 1)))
plot(g(c(-2.4, -1, 10, -1)))

# Example code that we didn't talk about in class
library(neuralnet)

# Put all variables together and rename for convenience
dataset <- ts.combine(dgdp, lags(dgdp,1), 
                      lags(ffr,1), lags(coreinf,1))
colnames(dataset)
colnames(dataset) <- c("dgdp", "dgdp1", "ffr1", "core1")

# Fit the model. This doesn't converge.
fit.nn <- neuralnet(dgdp ~ dgdp1 + ffr1 + core1, 
                    data=dataset, hidden=5)

# Set stepmax to allow more iterations. This converges.                    
fit.nn <- neuralnet(dgdp ~ dgdp1 + ffr1 + core1, 
                    data=dataset, hidden=5, stepmax=100000)
class(fit.nn)
names(fit.nn)

# Print the weights. The first column is the weights for A1 and so on.
fit.nn$weights

# This is the logistic activation function (doesn't support ReLU yet)
1/(1 + exp(-x))

# Here are the first weights
# [1,]  34.34542
# [2,] -84.72953
# [3,] 156.22399
# [4,] -22.10574

# Compute the first activation and plot it
z <- 34.34542 + dataset[,2:4] %*% c(-84.72953, 156.22399, -22.10574)
A1.raw <- 1/(1+exp(-z))
A1 <- ts(A1.raw, end=end(dataset), frequency=4)
plot(A1)

# Forecasting regression
tsreg(dgdp, A1)
