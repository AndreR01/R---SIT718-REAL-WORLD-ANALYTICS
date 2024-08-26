kei.data <-as.matrix(read.table("KeiHotels.txt"))
kei.data

minkowski <- function(x,y,p=1){
  sum(abs(x-y)^p)^(1/p)
}

minkowski (kei.data[,1],kei.data[,2],p=2)
minkowski (kei.data[,1],kei.data[,5],p=2)

cor(kei.data[,1],kei.data[,2],method="spearman")
cor(kei.data[,1],kei.data[,5],method="spearman")

cor(kei.data[,1],kei.data[,2],method="pearson")
cor(kei.data[,1],kei.data[,5],method="pearson")

cor(kei.data[,1],kei.data[,2]) #pearson is the default
cor(kei.data[,1],kei.data[,5])

mdist <-array(0,9)
edist <-array(0,9)
scorr <-array(0,9)
pcorr <-array(0,9)

#Calculate the Manhattan distance using for loop
for (i in 2:10){
  mdist[i-1] <-minkowski(kei.data[,1],kei.data[,i],p=1) #Calculate the Manhattan distance 
  edist[i-1] <-minkowski(kei.data[,1],kei.data[,i],p=2) #Calculate the Euclidean distance 
  scorr[i-1] <-cor(kei.data[,1],kei.data[,i],method="spearman") #Calculate the Spearman Correlation 
  pcorr[i-1] <-cor(kei.data[,1],kei.data[,i],method="pearson") #Calculate the Pearson Correlation
  }

stable <-rbind(mdist,edist,scorr,pcorr)
stable

par(mfrow=c(1,2))
hist(kei.data[,1])
#hist(kei.data[,2])
#hist(kei.data[,3])
hist(kei.data[,4])
#hist(kei.data[,5])

par(mfrow=c(1,3))
plot(kei.data[,1],kei.data[,2])
plot(kei.data[,1],kei.data[,3])
plot(kei.data[,1],kei.data[,4])
par(mfrow=c(1,3))
plot(kei.data[,1],kei.data[,5])
plot(kei.data[,1],kei.data[,6])
plot(kei.data[,1],kei.data[,7])
par(mfrow=c(1,3))
plot(kei.data[,1],kei.data[,8])
plot(kei.data[,1],kei.data[,9])
plot(kei.data[,1],kei.data[,1])


source("AggWaFit718.R")
fit.QAM(kei.data[,c(2:10,1)]) #Generates output files with weights and errors
source("AggWaFit718.R")
fit.QAM(kei.data[,c(2:10,1)],output.1="outputPM.txt",stats.1="statsPM.txt",g=PM05,g.inv=invPM05)

fit.OWA(kei.data[,c(2:10,1)],output.1="outputOWA.txt",stats.1="statsOWA.txt")

fit.QAM(kei.data[,c(2:10,1)],output.1="outputGM.txt",stats.1="statsGM.txt",g=GMa,g.inv=invGMa)

fit.choquet(kei.data[,c(6,5,10,9,1)],output.1="outputCho.txt",stats.1="statsCho.txt")

