## Do simulations to show what it means to have a variance that grows with T

set.seed(800)
output <- replicate(100, {
  sum(rnorm(100))
})
var(output)

set.seed(800)
output2 <- replicate(100, {
  sum(rnorm(500))
})
var(output2)
plot(output)
plot(output2)

set.seed(800)
output3 <- replicate(100, {
  sum(rnorm(5000))
})
var(output3)
plot(output3)

set.seed(800)
output4 <- replicate(100, {
  sum(rnorm(50000))
})
var(output4)
plot(output4)
