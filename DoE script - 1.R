#### One factor, multiple levels
appleData=read.csv(file=file.choose())
appleData
attach(appleData)

OneFactorModel=aov(Growth~Concentration) #Run the ANOVA
print(model.tables(OneFactorModel,"means"),digits=3)
boxplot(Growth~Concentration)
summary(OneFactorModel)

plot(rstandard(OneFactorModel)) #Plot residuals against index
abline(0,0)
hist(rstandard(OneFactorModel)) #Check histogram of residuals

t=qt(0.95,35) #critical t value with alpha = 0.05 and v degrees of freedom
sd=sqrt(2*168396/8) #standard deviation
LSD = t*sd


#### Two factors, multiple levels

#No significant interaction term
mothData=read.csv(file=file.choose())
attach(mothData)
boxplot(Moths~Location)
boxplot(Moths~Trap)
model2=aov(Moths~Location*Trap)
summary(model2)
with(mothData,interaction.plot(x.factor=Location,trace.factor=Trap,response=Moths,fun=mean,type="b",main="Interaction Plot"))

hist(rstandard(model2))

plot(rstandard(model2),Location,main="Residuals across locations")
plot(rstandard(model2),Trap,main="Residuals across trap")


##Interaction is significant
newsData=read.csv(file=file.choose())
newsData
attach(newsData)

boxplot(Inquiries~Day)
boxplot(Inquiries~Section)
print(model.tables(model3,"means"),digits=3)

model3=aov(Inquiries~Day*Section) #Run the ANOVA with interaction terms
summary(model3)
boxplot(Inquiries~Day*Section)
with(newsData,interaction.plot(x.factor=Day,trace.factor=Section,response=Inquiries,fun=mean,type="b",main="Interaction Plot"))

hist(rstandard(model3))

plot(rstandard(model3),Location,main="Residuals across day")
plot(rstandard(model3),Trap,main="Residuals across section")



## 2^2 design
DoEdata=read.csv(file=file.choose())
attach(DoEdata)

model=lm(Y~A+B+AB)
summary(model)

model=lm(Y~A*B) #Alternate way to write model with interaction term
summary(model)

model2=lm(Y~A+B) #Demonstrates orthogonality principle
summary(model2)


#Demonstrate orthogonality principle
MovieData = read.csv(file=file.choose())
attach(MovieData)
MovieModel = lm(gross~duration+budget+imdb_score+num_critic_for_reviews)
summary(MovieModel)

ModifiedModel = lm(gross~duration+budget+num_critic_for_reviews)
summary(ModifiedModel)

###3 factor design

rm(data)
Yoshi=read.csv(file=file.choose()) #Yoshi data
attach(Yoshi)

model3=lm(Y~A*B*C)
summary(model3)

model4=lm(Y~A+B+C+A*C)
summary(model4)

plot(rstandard(model3),A,main="Type of Pellets")
abline(0,0)
plot(rstandard(model3),B,main="Water Level")
abline(0,0)
plot(rstandard(model3),C,main="Temperature")
abline(0,0)

hist(rstandard(model3),breaks=5)




###Single replicate design

rm(data)
data=read.csv(file=file.choose()) #Single replicate data
attach(data)

model4=lm(Y~A*B*C*D)
summary(model4)

#Makes a normal probability plot
tmp <- qqnorm( coef(model4)[-1] )
qqline( coef(model4)[-1] )
text( tmp$x, tmp$y, names(coef(model4)[-1]), pos=3 )

finalmodel=lm(Y~C+D+C*D)
summary(finalmodel)

