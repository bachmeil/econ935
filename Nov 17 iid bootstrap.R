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

# Generate a new set of residuals
set.seed(100)
res.infsim <- sample(res.inf, replace=TRUE)
res.ffrsim <- sample(res.ffr, replace=TRUE)

# Generate a new sample of both variables using a loop
# (In general, avoid loops)
# Earliest observation of both variables is 1957:Q2
tsobs(coreinf, c(1957,2))
tsobs(ffr, c(1957,2))
inf.sim <- 3.24
ffr.sim <- 3.00
for (ii in 1:length(res.infsim)) {
  # Compute the next values of both variables
  # Using the estimated coefficients from above
  newinf <- 0.39 + 0.70*last(inf.sim) + 0.14*last(ffr.sim) + res.infsim[ii]
  newffr <- 0.16 - 0.05*last(inf.sim) + 1.00*last(ffr.sim) + res.ffrsim[ii]
  # Save the newly generated data
  inf.sim <- c(inf.sim, newinf)
  ffr.sim <- c(ffr.sim, newffr)
}

# Print out the simulated data
# as.double used to strip off weird namings attached by R
as.double(inf.sim)
as.double(ffr.sim)

# You now have a new simulated dataset
# You can use the simulated dataset to compute the IRFs
# Then you can generate another simulated dataset
# Do not reset the seed when you do that
# Otherwise you'll always generate exactly the same dataset