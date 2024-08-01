V <- read.table("volley.txt")
V[,1] = (V[,1]-min(V[,1]))/(max(V[,1])-min(V[,1]))
V[,2] = (V[,2]-mean(V[,2]))/sd(V[,2])
View(V)



