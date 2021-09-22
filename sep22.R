draw <- rnorm(100000)
plot(density(draw))
mean(draw)
sd(draw)
mean(draw < 1.96)
mean(draw < -1.96)

set.seed(200)
y <- rnorm(400)
x <- rnorm(400)
cor(y,x)

set.seed(200)
simoutput <- replicate(1000, {
  y <- rnorm(400)
  x <- rnorm(400)
  cor(y,x)
})
simoutput
plot(density(simoutput))
mean(simoutput)
sd(simoutput)
mean(abs(simoutput) < 0.1)
mean(abs(simoutput) < 0.2)

set.seed(200)
y <- arima.sim(list(ar=0.999), n=400)
x <- arima.sim(list(ar=0.999), n=400)
cor(y,x)

set.seed(200)
simoutput <- replicate(1000, {
  y <- arima.sim(list(ar=0.999), n=400)
  x <- arima.sim(list(ar=0.999), n=400)
  cor(y,x)
})
simoutput
plot(density(simoutput))
mean(simoutput)
sd(simoutput)
mean(abs(simoutput) < 0.1)
mean(abs(simoutput) < 0.2)
mean(abs(simoutput) < 0.8)
max(abs(simoutput))




