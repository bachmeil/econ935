# Linear algebra hack
# Define x and y
x <- matrix(c(1, 2.2, 4.4,
              0, 1, 5.5,
              0, 0, 1), ncol=3)
x
y <- diag(3)
diag(y) <- c(1.2, 2.4, 3.5)
y

# You only know M
M <- x %*% y %*% t(y) %*% t(x)
M

# Apply the Choleski to M
P <- t(chol(M))
P

# P is the square root of M in this sense
P %*% t(P)

# D is our y matrix from above
D <- diag(3)
diag(D) <- diag(P)
D

# This is the x matrix from above
P %*% solve(D)

# These things are equivalent
J <- x %*% y
J[,1]

E <- matrix(c(D[1,1],0,0))
E
x %*% E

# Three-variable SVAR model of GDP growth, the fed funds rate,
# and core CPI inflation
dgdp.raw <- read.csv("dgdp.csv", header=TRUE)
dgdp <- ts(dgdp.raw[,2], start=c(1947,2), frequency=4)
ffr.raw <- read.csv("fedfunds.csv", header=TRUE)
ffr <- ts(ffr.raw[,2], start=c(1954,3), frequency=4)
core.raw <- read.csv("corecpi.csv", header=TRUE)
core <- ts(core.raw[,2], start=c(1957,1),
           frequency=4)
coreinf <- 400*pctChange(core)
plot(coreinf)

# Estimate the reduced form VAR model
tsdata <- ts.combine(dgdp, coreinf, ffr)
varfit <- VAR(tsdata, lag.max=5, ic="AIC")

# Get the residuals
res <- residuals(varfit)
res

# Compute the Choleski decomposition of the 
# covariance matrix of the residuals
cov(res)
t(chol(cov(res)))

# Changing the ordering of the variables in
# tsdata changes the impact effects
tsdata <- ts.combine(coreinf, dgdp, ffr)
varfit <- VAR(tsdata, lag.max=5, ic="AIC")
res <- residuals(varfit)
t(chol(cov(res)))

# Estimate the SVAR model by OLS
rhs.lagged <- ts.combine(lags(dgdp,1:5),
                         lags(coreinf,1:5),
                         lags(ffr,1:5))
fit.inf <- tsreg(coreinf, ts.combine(
  dgdp, rhs.lagged))
fit.inf

# Effect of y shock on inflation
1.0629962*0.23937
fit.ffr <- tsreg(ffr, ts.combine(
  dgdp, coreinf, rhs.lagged))
fit.ffr

# ffr responds to both y and inflation
# That's why we have to account for both when using the FFR equation
0.234416*1.0629962*0.23937 + 0.124494*1.0629962

# The vars package gives (A^{-1} B)^{h} for any h
Phi(varfit)[,,2] %*% t(chol(cov(res)))

# Normally you will let the vars package do the calculations
# for you
plot(irf(varfit, impulse="dgdp", boot=FALSE, n.ahead=24))
?irf
z
z[,1]
z$dgdp[,1]
