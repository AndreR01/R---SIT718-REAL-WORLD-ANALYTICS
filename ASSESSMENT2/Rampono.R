################################# 
# You can use this template to draft the script for your Assessment 2 of SIT718.
# More clarification and related resources can be found at
# https://d2l.deakin.edu.au/d2l/le/content/1422222/viewContent/7266544/View
#################################

#############################################################################################
# save your code as "name-code.R" (where ''name'' is replaced with your surname or first name).
#############################################################################################

##################################
#Question 1 - Understand the Data
##################################
install.packages("ggplot2")
data.raw <- as.matrix(read.table("RedWine.txt"))

set.seed(244906702) # using your student ID number for reproducible sampling with the seed function

wines <- data.raw[sample(1:1599, 500), c(1:6)]

citric <-c(wines[,1])
chlorides <-c(wines[,2])
sulfur_dioxide <-c(wines[,3])
hydrogen <-c(wines[,4])
alcohol <-c(wines[,5])
quality <-c(wines[,6])




wines.header <- c("citric acid", "chlorides", "total sulfur dioxide", "pH", "alcohol", "quality")
colnames(wines) <- c("citric acid", "chlorides", "total sulfur dioxide", "pH", "alcohol", "quality")
pairs(wines[, c(1, 3:6)], main="Scatter Plot Matrix for W Dataset")



# Create 5 scatterplots function (for each X variable against the variable of interest Y) 
par(mfrow=c(2,3))
plot(citric, quality, main="Scatter plot of Citric Acid v Quatlity", xlab="Citric Acid",ylab="Quality")
plot(chlorides,quality,main="Scatter plot of Chlorides v Quatlity", xlab="Chlorides",ylab="Quality")
plot(sulfur_dioxide,quality,main="Scatter plot of Total Sulfur Dioxide v Quatlity", xlab="Total Sulfur Dioxide",ylab="Quality")
plot(hydrogen,quality,main="Scatter plot of pH v Quatlity", xlab="pH",ylab="Quality")
plot(alcohol,quality,main="Scatter plot of Alcohol v Quatlity", xlab="Alcohol",ylab="Quality")
cor(citric,quality)
cor.test(citric,quality)
cor(chlorides,quality)
cor(sulfur_dioxide,quality)
cor(hydrogen,quality)
cor(alcohol,quality)


# Create 6 histograms for each X variable and Y
par(mfrow=c(2,3))
hist(citric, xlab="Citric Acid (g/L)", ylab="Frequency (wine samples)", main="Citric Acid (grams/Litre)")
hist(chlorides, xlab="Chlorides (g/L)", ylab="Frequency (wine samples)", main="Chlorides (grams(sodium chloride)/Litre)")
hist(sulfur_dioxide, xlab="Total Sulfur Dioxide (mg/L)", ylab="Frequency (wine samples)", main="Total Sulfur Dioxide (milligrams/Litre)")
hist(hydrogen, xlab="pH", ylab="Frequency (wine samples)", main="pH")
hist(alcohol, xlab="Alcohol %", ylab="Frequency (wine samples)", main="Alcohol (volume as a %")
hist(quality, xlab="Quality", ylab="Frequency (wine samples)", main="Quatlity")


################################
#Question 2 - Transform the Data
################################
#Before fitting the models, the data was first standardized to a zero mean and one standard deviation

#Calculate mean and standard deviation for each variable
citric.mean <-mean(citric)
citric.mean
citric.sd <-sqrt(sum((citric-citric.mean)^2)/(length(citric)-1))
citric.sd

chlorides.mean <-mean(chlorides)
chlorides.mean
chlorides.sd <-sqrt(sum((chlorides-chlorides.mean)^2)/(length(chlorides)-1))
chlorides.sd

sulfur_dioxide.mean <-mean(sulfur_dioxide)
sulfur_dioxide.mean
sulfur_dioxide.sd <-sqrt(sum((sulfur_dioxide.mean)^2)/(length(sulfur_dioxide)-1))
sulfur_dioxide.sd


hydrogen.mean <-mean(hydrogen)
hydrogen.mean
hydrogen.sd <-sqrt(sum((hydrogen.mean)^2)/(length(hydrogen)-1))
hydrogen.sd

alcohol.mean <-mean(alcohol)
alcohol.mean
alcohol.sd <-sqrt(sum((alcohol.mean)^2)/(length(alcohol)-1))
alcohol.sd

quality.mean <-mean(quality)
quality.mean
quality.sd <-sqrt(sum((quality.mean)^2)/(length(quality)-1))
quality.sd

#KS test for all variables
ks.test(citric,"pnorm",mean=citric.mean,sd=citric.sd)
ks.test(chlorides, "pnorm", mean=chlorides.mean,sd=chlorides.sd)
ks.test(sulfur_dioxide,"pnorm", mean=sulfur_dioxide.mean,sd=sulfur_dioxide.sd)
ks.test(hydrogen,"pnorm", mean=hydrogen.mean,sd=hydrogen.sd)
ks.test(alcohol,"pnorm",mean=alcohol.mean,sd=alcohol.sd)
ks.test(quality,"pnorm",mean=quality.mean,sd=quality.sd)



I <- c("define your variable index") # Choose any four X variables and Y

variables_for_transform <- data.subset[,I]  # obtain a 400 by 5 matrix

# for each variable, you need to figure out a good data transformation method, 
# such as Polynomial, log and negation transformation. The k-S test and Skewness 
# calculation may be helpful to select the transformation method

 

p=0.5 # for example, using p=0.5 to transform the first variable. You should change p based on your distribution.
data.transformed[,1]=variables_for_transform[,1]^p 

# A Min-Max and/or Z-score transformation should then be used to adjust the scale of each variable

# min-max normalisation
minmax <- function(x){
  (x - min(x))/(max(x)-min(x))
}

# z-score standardisation and scaling to unit interval
unit.z <- function(x){
  0.15*((x-mean(x))/sd(x)) + 0.5
}

data.transformed[,1]=minmax(data.transformed[,1]) # for example,using min-max normalisation for the first varible.


# Save this transformed data to a text file
write.table(data.transformed, "name-transformed.txt")  # replace ??name?? with either your surname or first name.


##########################################
#Question 3 - Build models and investigate
##########################################

source("AggWaFit718.R")

data.transformed_copy <- as.matrix(read.table("name-transformed.txt"))  # import your saved data

# Get weights for Weighted Arithmetic Mean with fit.QAM() 


# Get weights for Power Mean p=0.5 with fit.QAM()


# Get weights for Power Mean p=2 with fit.QAM()


# Get weights for Ordered Weighted Average with fit.OWA()




#######################################
#Question 4 - Use Model for Prediction
#######################################

new_input <- c(0.9, 0.65, 38, 2.53, 7.1) 

new_input_for_transform <- new_input[c("index")] # choose the same four X variables as in Q2 



# transforming the four variables in the same way as in question 2 



# applying the transformed variables to the best model selected from Q3 for Y prediction



# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer


 

#############################################################################################
# References 
# Following Harvard style: https://www.deakin.edu.au/students/studying/study-support/referencing/harvard
#############################################################################################

# You must cite all the datasets and packages you used for this assessment. 
#
#