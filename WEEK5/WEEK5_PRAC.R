x <- c(2,5,1,7,6)
w <- c(5,4,2,1,3)
m <- mean(x)
WAM <- function(x,w){
  w <- w/sum(w) #normalise var w
  wm <-sum(w*x) #calculate weighted mean
  wm
}
wm <- wam(x,w)

c(m,wm)

# Power mean & Weighted Power Mean
PM <- function(x,p){
  if(p==0){
    prod(x)^(1/length(x))
  }else{
    (mean(x^p))^(1/p)
  }
}

PM(c(2,5,1,7,6),1)

# Weighted Power Mean
WPM <- function(x,p,w){
  w <- w/sum(w) #normalise var w
  if(p==0){
    prod(x^w)
  }else{
    sum(w*(x^p))^(1/p)
  }
}
WPM(x,1,w)
WPM

# Weighted Median

WMed <-function(x,w){
  #normalises the weights
  w <- w/sum(w)
  #get the order of x (smallest to biggest)
  od <- order(x)
  #rearrange x
  x <-x[od]
  #rearrange w
  w <-w[od]
  #Accumulate the weights from left to right
  #Check if it is == 0.5 or > 0.5 then return the weighted median
  
  w
  x
  acc.w <- 0
  i <- 1
  while(acc.w<0.5){
    acc.w <- acc.w + w[i]
    print(acc.w)
    i <- i+1
  }
  
  if(acc.w==0.5){
    WMed <- (x[i] + x[i-1])/2
  }else{
    WMed <- x[i-1]
  }
  print(WMed)
  }
x <- c(0.62,0.33,0.26,0.11,0.91,0.12)
w <- c(0.32,0.08,0.2,0.06,0.10,0.24)
WMed(x,w)

#BORDA COUNTS
BC <- function(x,w){
  print(x)
  total.scores <- array(0,nrow(x))
  print(total.scores)
  #Rank the column values
  for(j in 1:ncol(x)){
    x[,j] <-rank(-x[,j])
  }
  #Change the rank scores
  y <- x-x
  for(i in 1:length(w)){
    y[x==i] <- w[i]
  }
  for(i in 1:nrow(y)){
    total.scores[i] <- sum(y[i,])
  }
  total.scores
  }



x <- rbind(c(9,6,4),c(7,7,6),c(4,8,8))
w <- c(2,1,0)
BC(x,w)

#OWA OPERATOR

#TRIMMING
trmean <-mean(x,trim=0.1)
trmean
