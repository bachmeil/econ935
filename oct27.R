# Work with a bivariate SVAR model of GDP
# growth and the fed funds rate
dgdp.raw <- read.csv("dgdp.csv", header=TRUE)
dgdp <- ts(dgdp.raw[,2], start=c(1947,2), frequency=4)
ffr.raw <- read.csv("fedfunds.csv", header=TRUE)
ffr <- ts(ffr.raw[,2], start=c(1954,3), frequency=4)

# Estimate the SVAR model using equation by
# equation OLS
fit.dgdp <- tsreg(dgdp, 
                  ts.combine(lags(dgdp,1),
                             lags(ffr,1)))
coefficients(fit.dgdp)

# This equation has a time t regressor
fit.ffr <- tsreg(ffr, 
                  ts.combine(lags(dgdp,0:1),
                             lags(ffr,1)))
coefficients(fit.ffr)

# Put into the matrix form using the estimated
# coefficients from above
D <- matrix(c(0, 0.2, 0, 0), ncol=2)
D
A <- diag(2) - D
A
B <- matrix(c(0.061, 0.168, -0.03, 0.98),
            ncol=2)
B
E <- matrix(c(0, 1), ncol=1)
E

# Find the initial (impact) effects
impact <- solve(A) %*% E
impact

# Given the initial effects, compute
# the IRFs at longer horizons
irf1 <- solve(A) %*% B %*% impact
irf1
irf2 <- solve(A) %*% B %*% irf1
irf2
