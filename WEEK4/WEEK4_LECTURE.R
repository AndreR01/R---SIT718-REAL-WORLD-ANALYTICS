# Generate random number
set.seed(123)
sample(10)

# uniform distribution
data <-runif(n=1000, min=0, max=1)
data
hist(data)

# normal distribution
data1 <- rnorm(30,mean=0,sd=1)
data2 <- rnorm(1000,mean=0,sd=1)

par(mfrow=c(1,2))
hist(data1,main="30 samples")
hist(data2,main="1000 samples")

# CDF
dnorm(0) # height of CDF at 0 (mean)
pnorm(1) - pnorm(-1) # area of the curve between 1std and -1std

# Binomial distribution
set.seed(123)
sim <- rbinom(100, 10, 1/6)
table(sim)

set.seed(123)
sim2 <- rbinom(1000,10,1/2)
barplot(table(sim2), main="A simulation of 1000 people tossing a coin 10  times")

dbinom(1, 10, 1/6)

dbinom(0,10,1/6) + dbinom(1,10,1/6) + dbinom(2,10,1/6) + dbinom(3,10,1/6)

pbinom(3,10,1/6)

dbinom

# Poisson random variable
set.seed(123)
poisson <- rpois(1000, lambda=3)

hist(poisson, main="A histogram of a Poisson distribution")

dpois(2,lambda=3)

barplot(dpois(0:10,lambda=3), names=0:10,main="A barplot of a Poisson distribution")



        