This is a simple example to show how to use R to choose the number of lags in an ARMA model. It's slightly more involved than what you did before, when selecting the number of lags in an AR or MA model using `lapply`. In this case, you have to send two choices for each model, `p` and `q`.

There is a related function called `apply`. Actually, `lapply` is the version of apply that works on lists, hence the "l" at the start of the name. `apply` applies a function to each row or column of a matrix. An example should clarify.

```
potential.lags <- expand.grid(1:4, 1:4)
colnames(potential.lags) <- c("p", "q")
```

This returns a matrix where the rows are all combinations of the two arguments. Print it out to see what it looks like. Column `p` is the number of AR lags and `q` is the number of MA lags.

```
info.crit <- function(choice) {
    fit <- arima(y, order=c(choice["p"], 0 , choice["q"]))
    return(c(aic=AIC(fit), sic=BIC(fit)))
}
```

This is the same as the function I gave you before, with the only difference being that you need to send a choice of both `p` and `q`.

```
apply(potential.lags, MARGIN=1, info.crit)
```

When setting `MARGIN=1`, `apply` sends one row of `potential.lags` at a time to `info.crit`.