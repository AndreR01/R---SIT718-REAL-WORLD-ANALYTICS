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
hist(citric, xlab="Citric Acid (g/L)", ylab="Count", main="Citric Acid (grams/Litre)",col="#619CFF")
plot(citric, quality, main="Citric Acid (g/L) v Quatlity", xlab="Citric Acid (g/L)",ylab="Quality")
plot(chlorides,quality,main="Chlorides (g/L) v Quatlity", xlab="Chlorides (g/L)",ylab="Quality")
hist(chlorides, xlab="Chlorides (g/L)", ylab="Count", main="Chlorides (grams(sodium chloride)/Litre)",col="#619CFF")
plot(sulfur_dioxide,quality,main="Total Sulfur Dioxide (mg/L) v Quatlity", xlab="Total Sulfur Dioxide (mg/L)",ylab="Quality")
hist(sulfur_dioxide, xlab="Total Sulfur Dioxide (mg/L)", ylab="Count", main="Total Sulfur Dioxide (milligrams/Litre)",col="#619CFF")
plot(hydrogen,quality,main="pH v Quatlity", xlab="pH",ylab="Quality")
hist(hydrogen, xlab="pH", ylab="Count", main="pH",col="#619CFF")
plot(alcohol,quality,main="Alcohol (%) v Quatlity", xlab="Alcohol %",ylab="Quality")
hist(alcohol, xlab="Alcohol (%)", ylab="Count", main="Alcohol (%)",col="#619CFF")

hist(quality, xlab="Quality", ylab="Count", main="Quatlity",col="#619CFF")


#CORRELATIONS
#Pearson Correlation Coefficient between independent variables and quality 
cor(citric,quality)
cor(chlorides,quality)
cor(sulfur_dioxide,quality)
cor(hydrogen,quality)
cor(alcohol,quality)
cor(quality,quality)

################################
#Question 2 - Transform the Data
################################
#Before fitting the models, the data was first standardized to a zero mean and one standard deviation

#MEAN, MEDIANS & STD DEVIATION FOR VARIABLES
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
quality.median <-median(quality)
quality.median

wines.sample <-wines[,c(1,2,3,5,6)]
wines.sample

#FUNCTIONS
#Scaling using minimum-maximum
minmax <- function(x){
  (x - min(x))/(max(x)-min(x))
}

#Assign variables 
s_citric <-c(wines.sample[,1])
s_chlorides <-c(wines.sample[,2])
s_sulfur_dioxide <-c(wines.sample[,3])
s_hydrogen <-c(wines.sample[,3])
s_alcohol <-c(wines.sample[,4])
s_quality <-c(wines.sample[,5])

#CORRELATION
#Pearson Correlation Coefficient between sample of independent variables and quality 
#cor(s_citric,s_quality)
#cor(s_chlorides,s_quality)
#cor(s_sulfur_dioxide,s_quality)
#cor(s_alcohol,s_quality)




#MEAN, MEDIANS & STD DEVIATION FOR SAMPLE VARIABLES
s_citric_mean <-mean(s_citric)
s_chlorides_mean <-mean(s_chlorides)
s_sulfur_dioxide_mean <-mean(s_sulfur_dioxide)
s_hydrogen_mean <-mean(s_hydrogen)
s_alcohol_mean <-mean(s_alcohol)
s_quality_mean <-mean(s_quality)

s_citric_median <-median(s_citric)
s_chlorides_median <-median(s_chlorides)
s_sulfur_dioxide_median <-median(s_sulfur_dioxide)
s_hygrogen_median <-median(s_hydrogen)
s_alcohol_median <-median(s_alcohol)
s_quality_median <-median(s_quality)

s_citric_sd <-sd(s_citric)
s_chlorides_sd <-sd(s_chlorides)
s_sulfur_dioxide_sd <-sd(s_sulfur_dioxide)
s_hydrogen_sd <-sd(s_hydrogen)
s_alcohol_sd <-sd(s_alcohol)
s_quality_sd <-sd(s_quality)


s_citric_mean
s_citric_median
s_citric_sd

s_chlorides_mean
s_chlorides_median
s_chlorides_sd

s_sulfur_dioxide_mean
s_sulfur_dioxide_median
s_sulfur_dioxide_sd

s_hydrogen_mean
s_hygrogen_median
s_hydrogen_sd


s_alcohol_mean
s_alcohol_median
s_alcohol_sd

s_quality_mean
s_quality_median
s_quality_sd

#KS test for all variables
ks.test(s_citric,"pnorm",mean=s_citric_mean,sd=s_citric_sd)
ks.test(s_chlorides, "pnorm", mean=s_chlorides_mean,sd=s_chlorides_sd)
ks.test(s_sulfur_dioxide,"pnorm", mean=s_sulfur_dioxide_mean,sd=s_sulfur_dioxide_sd)
ks.test(s_hydrogen,"pnorm", mean=s_hydrogen_mean,sd=s_hydrogen_sd)
ks.test(s_alcohol,"pnorm",mean=s_alcohol_mean,sd=s_alcohol_sd)
ks.test(s_quality,"pnorm",mean=s_quality_mean,sd=s_quality_sd)


#TRANSFORMATION AND SCALING
#CITRIC - transformation using scaling and minmax
min_sqrt_citric <-min(sqrt(s_citric))
max_sqrt_citric <-max(sqrt(s_citric))
sqrt_citric <-((sqrt(s_citric)-min_sqrt_citric)/(max_sqrt_citric-min_sqrt_citric))

#CHLORIDES - use log transformation for chlorides due to strong positive skew
min_log <-min(log(s_chlorides))
max_log <-max(log(s_chlorides))
log_chlorides <- (1-((log(s_chlorides)-min_log)/(max_log-min_log)))

#TOTAL SULFUR DIOXIDE - use square root transformation for sulfur dioxide as not as skewed as chlorides
min_sqrt_sulfur_dioxide <-min(sqrt(s_sulfur_dioxide))
max_sqrt_sulfur_dioxide <-max(sqrt(s_sulfur_dioxide))
sqrt_sulfur_dioxide <- (1-((sqrt(s_sulfur_dioxide)-min_sqrt_sulfur_dioxide)/(max_sqrt_sulfur_dioxide-min_sqrt_sulfur_dioxide)))

#ALCOHOL - normalise alcohol with log transformation and standardise min/max
log_s_alcohol <-log(s_alcohol)
min_max_log_s_alcohol <-minmax(log_s_alcohol)

#QUALITY - linear scaling using minmax
min_max_quality <-minmax(s_quality)
min(min_max_quality)
max(min_max_quality)

#Assign transform variables to table
wines.sample[,1] <-c(sqrt_citric)
wines.sample[,2] <-c(log_chlorides)
wines.sample[,3] <-c(sqrt_sulfur_dioxide)
wines.sample[,4] <-c(min_max_log_s_alcohol)
wines.sample[,5] <-c(min_max_quality)
wines.sample

#EXPORT DATA
# Save this transformed data to a text file
write.table(wines.sample, "rampono_transformed.txt")  # replace ??name?? with either your surname or first name.

#TODO Do corrleation and Minkowski measures before or after tr
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
X1
X2 <-new_input[c(2)]
X2
X3 <-new_input[c(3)]
X3
X5 <-new_input[c(5)]
X5



# transforming the four variables in the same way as in question 2 
#CITRIC
new_X1 <-(sqrt(X1)-min_sqrt_citric)/(max_sqrt_citric-min_sqrt_citric)
new_X1


#CHLORIDES
new_X2 <- (1-((log(X2)-min_log)/(max_log-min_log)))
new_X2

#TOTAL SULFUR DIOXIDE
new_X3 <- (1-((sqrt(X3)-min_sqrt_sulfur_dioxide)/(max_sqrt_sulfur_dioxide-min_sqrt_sulfur_dioxide)))
new_X3

#ALCOHOL
new_X5 <- ((log(X5)-min(log_s_alcohol))/(max(log_s_alcohol)-min(log_s_alcohol)))
new_X5

# applying the transformed variables to the best model selected from Q3 for Y prediction



# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer
#REVERSE THE SCORES PROCESS TO CALCULTE THE QUALITY
q_score <-0.088937922
new_wine_quality <-(q_score*(max(s_quality)-min(s_quality)))+(min(s_quality))
round_new_wine_quality <-round(new_wine_quality,digits = 0)
round_new_wine_quality

#############################################################################################
# References 
# Following Harvard style: https://www.deakin.edu.au/students/studying/study-support/referencing/harvard
#############################################################################################

# You must cite all the datasets and packages you used for this assessment. 
# P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009.

#



#ggplot(wines, aes(x = alcohol, y = quality)) + geom_point()

#Transfrom citric acid by z-score standardisation and scaling to unit interval
#unit.z <- function(x){
#  0.15*((x-mean(x))/sd(x)) + 0.5
#}
#citric.z <-unit.z(s_citric)
#s_citric
#citric.z
#plot(citric.z, s_quality)
#plot(s_citric, s_quality)

#hist(citric.z)
#hist(s_citric)
#s_citric_m <-mean(s_citric)
#s_citric_sd <-sd(s_citric)
#ks.test(s_citric,"pnorm",mean=s_citric_m, sd=s_citric_sd)

#citric.z_m <-mean(citric.z)
#citric.z_sd <-sd(citric.z)
#ks.test(citric.z,"pnorm",mean=citric.z_m, sd=citric.z_sd)

#qqnorm(s_citric, main='Non-normal')
#qqline(s_citric)

#qqnorm(citric.z, main='Non-normal')
#qqline(citric.z)

#unit.z <- function(x){
#  0.15*((x-mean(x))/sd(x)) + 0.5
#}

#Use square root transformation for alcohol as not as skewed as chlorides
#min_sqrt_hydrogen <-sqrt(min(wines.sample[,4]))
#max_sqrt_hydrogen <-sqrt(max(wines.sample[,4]))
#sqrt_hydrogen <- (1-((sqrt(wines.sample[,4])-min_sqrt_hydrogen)/(max_sqrt_hydrogen-min_sqrt_hydrogen)))
#sqrt_hydrogen
#plot(sqrt_hydrogen,s_quality,main="Scatter plot of pH v Quatlity", xlab="pH",ylab="Quality")
#hist(sqrt_hydrogen, xlab="Alcohol %", ylab="Frequency (wine samples)", main="Alcohol (volume as a %")
#sqrt_hydrogen.mean <-mean(sqrt_hydrogen)
#sqrt_hydrogen.mean
#sqrt_hydrogen.sd <-sqrt(sum((sqrt_hydrogen.mean)^2)/(length(sqrt_hydrogen)-1))
#sqrt_alcohol.sd
#ks.test(sqrt_hydrogen,"pnorm",mean=sqrt_hydrogen.mean,sd=sqrt_alcohol.sd)



