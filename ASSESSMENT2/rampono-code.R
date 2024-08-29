#Author: Andre Rampono


##################################
#Question 1 - Understand the Data
##################################
install.packages("ggplot2")
library(ggplot2)
data.raw <- as.matrix(read.table("RedWine.txt"))
set.seed(244906702) # using your student ID number for reproducible sampling with the seed function
wines <- data.raw[sample(1:1599, 500), c(1:6)]

#Assign variables to names
citric <-c(wines[,1])
chlorides <-c(wines[,2])
sulfur_dioxide <-c(wines[,3])
hydrogen <-c(wines[,4])
alcohol <-c(wines[,5])
quality <-c(wines[,6])

wines.header <- c("citric acid", "chlorides", "total sulfur dioxide", "pH", "alcohol", "quality")
colnames(wines) <- c("citric acid", "chlorides", "total sulfur dioxide", "pH", "alcohol", "quality")

#SCATTTERPLOTS & HISTOGRAMS
# Create 5 scatterplots function (for each X variable against the variable of interest Y)
# Create 6 histograms for each X variable and Y
par(mfrow=c(1,2))
hist(chlorides, xlab="Chlorides (g/L)", ylab="Count", main="Chlorides (grams(sodium chloride)/Litre)",col="#619CFF")
plot(chlorides,quality,main="Chlorides (g/L) v Quatlity", xlab="Chlorides (g/L)",ylab="Quality")

hist(sulfur_dioxide, xlab="Total Sulfur Dioxide (mg/L)", ylab="Count", main="Total Sulfur Dioxide (milligrams/Litre)",col="#619CFF")
plot(sulfur_dioxide,quality,main="Total Sulfur Dioxide (mg/L) v Quatlity", xlab="Total Sulfur Dioxide (mg/L)",ylab="Quality")

hist(alcohol, xlab="Alcohol (%)", ylab="Count", main="Alcohol (%)",col="#619CFF")
plot(alcohol,quality,main="Alcohol (%) v Quatlity", xlab="Alcohol %",ylab="Quality")

hist(citric, xlab="Citric Acid (g/L)", ylab="Count", main="Citric Acid (grams/Litre)",col="#619CFF")
plot(citric, quality, main="Citric Acid (g/L) v Quatlity", xlab="Citric Acid (g/L)",ylab="Quality")

hist(hydrogen, xlab="pH", ylab="Count", main="pH",col="#619CFF")
plot(hydrogen,quality,main="pH v Quatlity", xlab="pH",ylab="Quality")

par(mfrow=c(1,1))
hist(quality, xlab="Quality", ylab="Count", main="Quatlity",col="#619CFF")


#CORRELATIONS
#Pearson Correlation Coefficient between independent variables and quality 
#cor(citric,quality)
#@cor(chlorides,quality)
#cor(sulfur_dioxide,quality)
#cor(hydrogen,quality)
#cor(alcohol,quality)
#cor(quality,quality)

################################
#Question 2 - Transform the Data
################################
#MEAN, MEDIANS & STD DEVIATION FOR VARIABLES
#Calculate mean and standard deviation for each variable
citric.mean <-mean(citric)
citric.sd <-sd(citric)
citric.median <-median(citric)

chlorides.mean <-mean(chlorides)
chlorides.sd <-sd(chlorides)
chlorides.median <-median(chlorides)

sulfur_dioxide.mean <-mean(sulfur_dioxide)
sulfur_dioxide.sd <-sd(sulfur_dioxide)
sulfur_dioxide.median <-median(sulfur_dioxide)

hydrogen.mean <-mean(hydrogen)
hydrogen.sd <-sd(hydrogen)
hydrogen.median <-median(hydrogen)

alcohol.mean <-mean(alcohol)
alcohol.sd <-sd(alcohol)
alcohol.median <-median(alcohol)

quality.mean <-mean(quality)
quality.sd <-sd(quality)
quality.median <-median(quality)

wines.sample <-wines[,c(1,2,3,5,6)]

#FUNCTIONS
#Scaling using minimum-maximum
minmax <- function(x){
  (x - min(x))/(max(x)-min(x))
}

#Assign variables 
s_citric <-c(wines.sample[,1])
s_chlorides <-c(wines.sample[,2])
s_sulfur_dioxide <-c(wines.sample[,3])
s_alcohol <-c(wines.sample[,4])
s_quality <-c(wines.sample[,5])

#MEAN, MEDIANS & STD DEVIATION FOR SAMPLE VARIABLES
s_citric_mean <-mean(s_citric)
s_chlorides_mean <-mean(s_chlorides)
s_sulfur_dioxide_mean <-mean(s_sulfur_dioxide)
hydrogen_mean <-mean(hydrogen)
s_alcohol_mean <-mean(s_alcohol)
s_quality_mean <-mean(s_quality)

s_citric_median <-median(s_citric)
s_chlorides_median <-median(s_chlorides)
s_sulfur_dioxide_median <-median(s_sulfur_dioxide)
hygrogen_median <-median(hydrogen)
s_alcohol_median <-median(s_alcohol)
s_quality_median <-median(s_quality)

s_citric_sd <-sd(s_citric)
s_chlorides_sd <-sd(s_chlorides)
s_sulfur_dioxide_sd <-sd(s_sulfur_dioxide)
hydrogen_sd <-sd(hydrogen)
s_alcohol_sd <-sd(s_alcohol)
s_quality_sd <-sd(s_quality)

#KS test for all variables
ks_citric <-ks.test(s_citric,"pnorm",mean=s_citric_mean,sd=s_citric_sd)
ks_chlordes <-ks.test(s_chlorides, "pnorm", mean=s_chlorides_mean,sd=s_chlorides_sd)
ks_sulfurd <-ks.test(s_sulfur_dioxide,"pnorm", mean=s_sulfur_dioxide_mean,sd=s_sulfur_dioxide_sd)
ks_hydrogen <-ks.test(hydrogen,"pnorm", mean=hydrogen_mean,sd=hydrogen_sd)
ks_alcohol <-ks.test(s_alcohol,"pnorm",mean=s_alcohol_mean,sd=s_alcohol_sd)
ks_quality <-ks.test(s_quality,"pnorm",mean=s_quality_mean,sd=s_quality_sd)


#TRANSFORMATION AND SCALING
#CITRIC - transformation using scaling and minmax
min_sqrt_citric <-min(sqrt(s_citric))
max_sqrt_citric <-max(sqrt(s_citric))
sqrt_citric <-((sqrt(s_citric)-min_sqrt_citric)/(max_sqrt_citric-min_sqrt_citric))

#CHLORIDES - use log transformation for chlorides due to strong positive skew
min_log <-min(log(s_chlorides,10))
max_log <-max(log(s_chlorides,10))
log_chlorides <- (1-((log(s_chlorides,10)-min_log)/(max_log-min_log)))

#TOTAL SULFUR DIOXIDE - use square root transformation for sulfur dioxide as not as skewed as chlorides
min_sqrt_sulfur_dioxide <-min(sqrt(s_sulfur_dioxide))
max_sqrt_sulfur_dioxide <-max(sqrt(s_sulfur_dioxide))
sqrt_sulfur_dioxide <- (1-((sqrt(s_sulfur_dioxide)-min_sqrt_sulfur_dioxide)/(max_sqrt_sulfur_dioxide-min_sqrt_sulfur_dioxide)))

#ALCOHOL - log transformation and scaling using min/max
log_s_alcohol <-log(s_alcohol,10)
min_max_log_s_alcohol <-minmax(log_s_alcohol)

#QUALITY - log transformation and scaling using min/max
log_s_quality <-log(s_quality,10)
min_max_log_s_quality <-minmax(log_s_quality)


#Assign transform variables to table
wines.sample[,1] <-c(sqrt_citric)
wines.sample[,2] <-c(log_chlorides)
wines.sample[,3] <-c(sqrt_sulfur_dioxide)
wines.sample[,4] <-c(min_max_log_s_alcohol)
wines.sample[,5] <-c(min_max_log_s_quality)

#EXPORT DATA
# Save this transformed data to a text file
write.table(wines.sample, "rampono_transformed.txt")  # replace ??name?? with either your surname or first name.


##########################################
#Question 3 - Build models and investigate
##########################################

source("AggWaFit718.R")

rampono_transformed_copy <- as.matrix(read.table("rampono_transformed.txt"))  # import your saved data

# Get weights for Weighted Arithmetic Mean with fit.QAM() 
fit.QAM(rampono_transformed_copy[,c(1:4,5)]) #Generates output files with weights and errors

# Get weights for Power Mean p=0.5 with fit.QAM()
fit.QAM(rampono_transformed_copy[,c(1:4,5)],output.1="outputPM05.txt",stats.1="statsPM05.txt",g=PM05,g.inv=invPM05)

# Get weights for Power Mean p=2 with fit.QAM()
fit.QAM(rampono_transformed_copy[,c(1:4,5)],output.1="outputPM2.txt",stats.1="statsPM2.txt",g=PM2,g.inv=invPM2)


# Get weights for Ordered Weighted Average with fit.OWA()
fit.OWA(rampono_transformed_copy[,c(1:4,5)],output.1="outputOWA.txt",stats.1="statsOWA.txt")

#######################################
#Question 4 - Use Model for Prediction
#######################################

new_input <- c(0.9, 0.65, 38, 2.53, 7.1) 

X1 <- new_input[c(1)] # choose the same four X variables as in Q2 
X2 <-new_input[c(2)]
X3 <-new_input[c(3)]
X5 <-new_input[c(5)]

# transforming the four variables in the same way as in question 2 
#CITRIC
new_X1 <-(sqrt(X1)-min_sqrt_citric)/(max_sqrt_citric-min_sqrt_citric)

#CHLORIDES
new_X2 <- (1-((log(X2,10)-min_log)/(max_log-min_log)))

#TOTAL SULFUR DIOXIDE
new_X3 <- (1-((sqrt(X3)-min_sqrt_sulfur_dioxide)/(max_sqrt_sulfur_dioxide-min_sqrt_sulfur_dioxide)))

#ALCOHOL
new_X5 <- ((log(X5,10)-min(log_s_alcohol))/(max(log_s_alcohol)-min(log_s_alcohol)))

# applying the transformed variables to the best model selected from Q3 for Y prediction


# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer
#REVERSE THE SCORES PROCESS TO CALCULTE THE QUALITY
q_score <-(0.20642723)
new_wine <-q_score*(max(log(s_quality,10))-min(log(s_quality,10)))+min(log(s_quality,10))
q_wine<-(10^new_wine)
round_q_wine <-round(q_wine,digits = 0)
round_q_wine



#############################################################################################
# References 
# Following Harvard style: https://www.deakin.edu.au/students/studying/study-support/referencing/harvard
#############################################################################################

# You must cite all the datasets and packages you used for this assessment. 
# P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009.