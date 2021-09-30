# September 29

We used the `tsgen` function in class. Here's a quick explanation of that function.

The goal is to make it easy to simulate from an arbitrary time series process without having to iterate over observations, store data, and that sort of thing. In 2021, it's better to use languages and libraries that automate those things. Having to write out those details yourself every time leads to bugs and a lot of wasted effort.

The arguments to `tsgen(n, f, init, trend=FALSE)` are

- n: The number of observations you want to generate.
- f: The function that generates one observation of the simulated data.
- init: The initial values of the simulated data. You need to include enough initial values to generate the first observation.
- trend: (Optional) If `TRUE`, there's a linear trend in the process, so send the observation number to `f`. The default value is `FALSE`, so if you only pass three arguments it is assumed there is no linear time trend.

Examples of `tsgen` usage are below, since we used it during the lecture.

## Plotting under the null hypothesis

To get a handle on what a time series plot looks like with a stochastic trend, we simulated data from random walk models with different deterministic terms.

```
library(tstools)
rw.pure <- function(y) {
  last(y) + rnorm(1)
}
set.seed(100)
plot(tsgen(1000, rw.pure, 0.0))

rw.drift <- function(y) {
  1.5 + last(y) + rnorm(1)
}
set.seed(100)
plot(tsgen(1000, rw.drift, 0.0))

rw.trend <- function(y, tr) {
  1.5 + last(y) + 0.01*tr + rnorm(1)
}
set.seed(100)
plot((1000, rw.trend, 0.0, TRUE))
```

Then we did the same thing, except where the simulated series is stationary but very persistent.

```
ar.pure <- function(y) {
  0.95*last(y) + rnorm(1)
}
set.seed(100)
plot(tsgen(1000, ar.pure, 0.0))

ar.drift <- function(y) {
  1.5 + 0.95*last(y) + rnorm(1)
}
set.seed(100)
plot(tsgen(1000, ar.drift, 0.0))

ar.trend <- function(y, tr) {
  1.5 + 0.95*last(y) + 0.01*tr + rnorm(1)
}
set.seed(100)
plot(tsgen(1000, ar.trend, 0.0, TRUE))
```

We did some Dickey-Fuller unit root tests.

```
library(urca)
data(Raotbl3)
attach(Raotbl3)
lc <- ts(lc, start=c(1966,4), frequency=4)
plot(lc)
plot(diff(lc))
length(lc)
fit <- tsreg(lc, ts(1:99, start=c(1966,4),
                    frequency=4))
fit
plot(fit$resids)
lc.df <- ur.df(y=lc, type="none", 
               selectlags="AIC")
summary(lc.df)
lc.df <- ur.df(y=lc, type="drift", 
               selectlags="BIC")
summary(lc.df)
lc.df <- ur.df(y=lc, type="trend", lags=0)
summary(lc.df)
```

We did a simulation to see what happens if there's a structural break in the coefficients of a stationary series.

```
set.seed(200)
coef <- replicate(400, {
  ysim1 <- tsgen(100, regime1, 0.0)
  ysim2 <- tsgen(150, regime2, last(ysim1))
  ysim <- ts(c(ysim1, ysim2))
  fit <- tsreg(ysim, lags(ysim,1))
  coefficients(fit)[2]
})
plot(density(coef))
```

