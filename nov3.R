# See the process when we know the shocks and coefficients
# This is A^{-1}
Ainv <- matrix(c(1, 2.2, 0.8,
                 0, 1, 0.9,
                 0, 0, 1), ncol=3)
Ainv
et <- Ainv %*% matrix(c(0.8, -0.3, -1.6))
solve(Ainv) %*% et

# Generate some structural shocks
# Have to be uncorrelated and serially uncorrelated
set.seed(100)
shocks <- matrix(rnorm(300), ncol=3)
shocks
Ainv %*% shocks[27,]

# Generate the reduced form residuals using the formula from class
rfres <- t(Ainv %*% t(shocks))
rfres

# Now reverse the process to get an estimate of A^{-1}
# This cov matrix is based on only 100 observations
cov(rfres)
M <- t(chol(cov(rfres)))
D <- diag(3)
diag(D) <- diag(M)
D

# Our estimate of A
Ainv.hat <- M %*% solve(D)
A.hat <- solve(Ainv.hat)

# Recover the estimated structural shocks
s.hat <- t(A.hat %*% t(rfres))
s.hat

# Now do the same thing using our data
library(tstools)
library(vars)
dgdp.raw <- read.csv("dgdp.csv", header=TRUE)
dgdp <- ts(dgdp.raw[,2], start=c(1947,2), frequency=4)
ffr.raw <- read.csv("fedfunds.csv", header=TRUE)
ffr <- ts(ffr.raw[,2], start=c(1954,3), frequency=4)
core.raw <- read.csv("corecpi.csv", header=TRUE)
core <- ts(core.raw[,2], start=c(1957,1),
           frequency=4)
coreinf <- 400*pctChange(core)
tsdata <- ts.combine(dgdp, coreinf, ffr)
varfit <- VAR(tsdata, lag.max=5, ic="AIC")
res <- residuals(varfit)

# Take the Choleski and recover A^{-1}
M <- t(chol(cov(res)))
M
D <- diag(3)
diag(D) <- diag(M)
D
Ainv.hat <- M %*% solve(D)
A.hat <- solve(Ainv.hat)
A.hat
res

# Solve out for the structural shocks
s.hat <- t(A.hat %*% t(res))
s.hat

# Plot the reduced form residual for the FFR
plot(res[,3], type="l")

# Plot the identified monetary policy shocks
plot(s.hat[,3], type="l")

# Plot the cumulative identified monetary policy shocks
plot(cumsum(s.hat[,3]), type="l")

# Do the same for the inflation and output shocks
plot(s.hat[,2], type="l")
plot(cumsum(s.hat[,2]), type="l")
plot(s.hat[,1], type="l")
plot(cumsum(s.hat[,1]), type="l")
