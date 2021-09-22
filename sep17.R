library(urca)
data("Raotbl3")
Raotbl3
lc <- ts(Raotbl3$lc, start=c(1966,4),
         frequency=4)
dlc <- diff(lc)
plot(dlc)

library(fGarch)
fit <- garchFit(~ garch(1,1), data=dlc)
summary(fit)
fit@fitted
fit@h.t
plot(ts(fit@h.t))
predict(fit, n.ahead=4)

fit <- garchFit(~ garch(3,0), data=dlc)
summary(fit)

fit <- garchFit(~ arma(2,3) + garch(1,1),
                data=dlc)
summary(fit)
