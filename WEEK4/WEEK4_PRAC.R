V <- read.table("volley.txt")
original <- V
V$V1
index <-1
c(min(V[index],max[V,index]))

#V[,1] <- original[,1]
#hist(V[,1],xlab="Sprints")
V[,1] <- (length(V[,1])-rank(V[,1]))/length(V[,1]-1)
hist(V[,1],xlab="Sprints ranked")

V[,1] = (V[,1]-mean(V[,1]))/sd(V[,1])
V[,1] = (V[,1]-min(V[,1]))/(max(V[,1])-min(V[,1]))
hist(V[,1],xlab="Sprints sd")
     
     
     
V[,1] <- (length(V[,1])-rank(V[,1]))/length(V[,1]-1)

plot(V[,1],xlab=("Rank based scores"))

plot(V[,3],xlab="Serves landing in")
plot(V[,4],xlab="Endurance")
hist(V[,3],xlab="Serves landing in")
hist(V[,4],xlab="Endurance")
V[,1] = (V[,1]-min(V[,1]))/(max(V[,1])-min(V[,1]))
V[,2] = (V[,2]-mean(V[,2]))/sd(V[,2])
V[,2] = (V[,2]-min(V[,2]))/(max(V[,2])-min(V[,2]))
V[,3] = (V[,3]-min(V[,3]))/(max(V[,3])-min(V[,3]))
V[,4] = (V[,4]-min(V[,4]))/(max(V[,4])-min(V[,4]))

View(V)

# negation transformation
min.sprint <- min(V[,1])
max.sprint <- max(V[,1])
V[,1] <- max.sprint - V[,1] + min.sprint

# min-max scale
for(index in 1:ncol(v)){
  min.val <- min(V[index])
  max.val <- max(v[index])
  V[,index] <- (V[,index] - min.val)/(max.val-min.val)
}

# z-score
V <- original
for (index in 1:ncol(V)){
  mean.val <- mean(V[,index])
  sd.val <- sd(V[,index])
  v[,index] <-(V[,index]-mean.val)/sd.val
}

z_score_fun <- function(column){
  column <- (column - mean(column))/sd(column)
}

for (i in 1:ncol(v))
{
  v[,i] <- z_score_fun(v[,i])
}