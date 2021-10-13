causality(varfit, cause="inf")
causality(varfit, cause="u")

varfcst <- function(t) {
  ds <- window(phillips, end=t)
  fit <- VAR(ds, lag.max=6, ic="SC")
  return(getVarForecast(fit, "inf", 1))
}
varfcst(c(2012,4))

dates <- make.dates(c(2009,12), c(2019,11),
                    12)
dates
varfcst(dates[1])
varfcst(dates[2])
varfcst(dates[3])
