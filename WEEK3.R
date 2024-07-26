data<- read.table("volley.txt")
data
#Visualise the histogram of the height
hist(data$V2,main="Histogram of Height",xlab="Height")

height <- data$V2
sprint <- data$V1

plot(height,sprint)

# Correlation
cor(height,sprint)

# All correlations (for all variables)
cor(data)

# Install packages for plotting
install.packages("ggcorrplot")
