# Show how to use the iid bootstrap to simulate one dataset

library(tstools)
library(vars)

# Load the data from Nov 3 (using two variables)
ffr.raw <- read.csv("fedfunds.csv", header=TRUE)
ffr <- ts(ffr.raw[,2], start=c(1954,3), frequency=4)
core.raw <- read.csv("corecpi.csv", header=TRUE)
core <- ts(core.raw[,2], start=c(1957,1),
           frequency=4)
coreinf <- 400*pctChange(core)
tsdata <- ts.combine(coreinf, ffr)

# Estimate the reduced form VAR with 1 lag
varfit <- VAR(tsdata, p=1)

# Print out the estimated model
varfit

# Compute the residuals of each equation
res <- residuals(varfit)
res.inf <- res[,"coreinf"]
res.ffr <- res[,"ffr"]

# Compute the fitted values of each variable
fitted.inf <- fitted(varfit)[,"coreinf"]
fitted.ffr <- fitted(varfit)[,"ffr"]

# Generate draws from the pick distribution
set.seed(100)
z <- sample(c(-1,1), size=257, replace=TRUE)

# Generate the new residuals
res.infsim <- z*res.inf
res.ffrsim <- z*res.ffr

# Generate a new sample of both variables
inf.sim <- fitted.inf + res.infsim
ffr.sim <- fitted.ffr + res.ffrsim
inf.sim
ffr.sim
