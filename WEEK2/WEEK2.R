mean(c(1,5,2,6))
min(c(4,6,2,7,3))

negSum <- function(arrayOfNums){
  result <- 0
  for (i in c(1:length(arrayOfNums))){
    if(arrayOfNums[i] < 0){
      result = result + arrayOfNums[i]
    }
  }
  result
}

negSum(c(-1,-2,4,5,6,-3))

sum_neg <- function(x){
  sum(x[x<0])
}
# Breaking down code
negative.indices <- x<0
x[negative.indices]
sum(x[negative.indices])

# Geometric Mean
geoMean <- function(x){
  s<-prod(x)
  n<-length(x)
  result<-s^(1/n)
  result
}

geoMean(c(1.09, 1.13, 1.02))

# Harmonic Mean
harMean <- function(x){
  n<-length(x)
  s<-(1/x)
  result<-n/sum(s)
  result
}

harMean(c(1.09, 1.13, 1.02))
values <- runif(1000, min=1, max=50)
hist(values)