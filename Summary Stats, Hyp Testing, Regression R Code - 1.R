
##### Summary statistics and visualization

mydata=read.csv(file=file.choose()) #Brings pop up box to select .csv data file
mydata #Displays all data
attach(mydata) #Makes columns accessible by name

head(mydata) #Displays column names and first 6 data points
nrow(mydata) #Reports number of observations (data points)
ncol(mydata) #Reports number of variables per observation

summary(mydata) #Reports Minimum, Maximum, Mean, and Q1,Median, and Q3 for each data column

mean(mydata$Time) #Reports mean of Time from dataset 
min(Time) #Reports minimum of Time from dataset
max(Time) #Reports maximum of Time from dataset
range(Time)

####### Visualization

boxplot(mydata) #Creates generic boxplot
boxplot(mydata[,1:2],main="Time to deliver package",xlab="Postman") #Creates boxplot with times only, adds title and x label

hist(Time) #Creates naive histogram of times
hist(Time,breaks=16) #Creates a histogram with 16 bins

plot(houses,Time)
cov(houses,Time)


######## Confidence intervals

alpha=.05
xbar=mean(Time) #mean
s=sd(Time) #Standard deviation
n=length(Time) #number of observations
t=qt(1-alpha/2, df=n-1) #alpha/2 t value with n-1 degrees of freedom
z=qnorm(1-alpha/2) #alpha/2 z value

lower = xbar-t*s/sqrt(n) #lower confidence bound
upper = xbar+t*s/sqrt(n) #upper confidence bound


####### T-tests

#One sample t-test

t.test(Time,mu=30) # H0: mu = 30 H1: mu /= 30
t.test(Time,mu=28,alternative="greater") # H0: mu = 28 H1: mu > 28


#Two sample t-test with equal variance
#alternative can be "two.sided","greater",or"less"
#mu is the hypothesized difference between means of populations
#Equal variance True/False
#Confidence level for fixed test
#paired test True/False

t.test(Time,Time2,alternative="two.sided",mu=0,var.equal=F,conf.level=0.95,paired=F)

#For paired t-test, it is often convenient to look at boxplots or histograms of the differences
boxplot(Time-Time2)
hist(Time-Time2)

#Another way to perform paired test
t.test(Time-Time2,alternative="t",mu=0,var.equal=F,conf.level=0.95)


##### ANOVA (Analysis of variance)

Mileage = c(354,363,381,382,370,364,370,382,373)
Octane = as.factor(c(87,87,87,89,89,89,93,93,93)) #cast treatment labels as factor
fit = aov(Mileage~Octane)
summary(fit)

datafilename="http://personality-project.org/r/datasets/R.appendix1.data" #read data from URL link
data.ex1=read.table(datafilename,header=T)   #read the data into a table

aov.ex1 = aov(Alertness~Dosage,data=data.ex1)  #do the analysis of variance
summary(aov.ex1)                                    #show the summary table
print(model.tables(aov.ex1,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(Alertness~Dosage,data=data.ex1)        #graphical summary


datafilename="http://personality-project.org/r/datasets/R.appendix2.data"
data.ex2=read.table(datafilename,header=T)   #read the data into a table
data.ex2                                      #show the data
aov.ex2 = aov(Alertness~Gender+Dosage,data=data.ex2)         #do the analysis of variance
summary(aov.ex2)                                    #show the summary table
print(model.tables(aov.ex2,"means"),digits=3)       #report the means and the number of subjects/cell
boxplot(Alertness~Dosage+Gender,data=data.ex2) #graphical summary of means of the 4 cells


##### Regression

model1=lm(Time~kms) #Performs simple linear regression using kms as predictor and Time as response
model1
summary(model1)

#Plots data and adds linear regression line
plot(kms,Time)
abline(model1)

#Calculates variance of errors, sigma^2
summary(model1)$sigma^2

#Check histogram and QQ plot of residuals
hist(rstandard(model1))
qqnorm(rstandard(model1))
qqline(rstandard(model1))

#Check for correlation of errors
plot(rstandard(model1))
abline(0,0)


#Multivariate linear model
model2=lm(Time~kms+houses+pieces)
summary(model2)

#Checking Fit of Model

#Creates plot of standardized residuals against index number
plot(rstandard(model2))
abline(0,0)

#Plots standardized residuals against predictor variable kms
plot(kms,rstandard(model2))
abline(0,0)

#Plots standardized residuals against predictor variable houses
plot(houses,rstandard(model2))
abline(0,0)

#Plots standardized residuals against predictor variable pieces
plot(pieces,rstandard(model2))
abline(0,0)

#Creates histogram of standardized residuals to check for normality
hist(rstandard(model2))

#Create q-q plot to check for normality
qqnorm(rstandard(model2))
qqline(rstandard(model2))