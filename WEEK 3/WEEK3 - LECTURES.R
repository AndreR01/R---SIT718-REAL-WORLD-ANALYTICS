barplot(c(1/6, 1/6, 1/6, 1/6, 1/6, 1/6),
names.arg=c("1","2","3","4","5","6"),
main="Probability of Each Number of Dice Occurning on a Single Dice Roll",xlab="Dice Number",ylab="Probability",ylim=c(0,1))


plot(c(1:6),
     array(1/5, 6),
     type="l",
     xlim=c(0,7),
     ylim=c(0,1),
     xlab="Value",
     ylab="Probability",
     main="Probability of Picking a Real Number Between 1 and 6"
)
polygon(c(1,1,6,6), c(0,0.2, 0.2, 0), angle=30, density=10)

normalData <- rnorm(1000,5.65,2.8)
hist(normalData)

standardisedData <- (normalData - 5.65)/2.8
stackloss(standardisedData)

read.table("volley.txt", header=FALSE, col.names=c("SPRINT","HEIGHT","WINNER","ENDURANCE"))
