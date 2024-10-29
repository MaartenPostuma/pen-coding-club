rm(list=ls())
library(ggplot2)
#colauttilab RIntroStats

#I am not fantastic at statistics but this guy is: https://colauttilab.github.io/RIntroStats/3_LinearModels.html 
#He also has a course crash course in R for biologists that I highly recommend! 
#The examples I use here are from the link above I would recommend checking it out!


set.seed(123) 
#What is a cool !continuous! yield variable
yield<-rnorm(1000) #Create a random distribution of yield values 
set.seed(234)

#What is a cool !continous! soilMoisture variable
soilMoisture<-rnorm(1000)  #Create a continuous normal distribution as x variable
ferti<-yield+rnorm(length(yield))#Make another continous variable (which is significant)

set.seed(345)

#What is a cool !Catagorical! variable
elevation<-sample(c("High","Medium","Low"),1000, replace=T) #Create an catagorical variable
dataExample<-data.frame(yield,soilMoisture,elevation,ferti) #Combine in a dataframe.
head(dataExample) 


plot(yield~soilMoisture,data=dataExample)

#How to plot in ggplot?
ggplot(dataExample,aes(y=yield,x=soilMoisture))+geom_point()

lm1<-lm(yield~soilMoisture,data=dataExample) #Relatively uninformative
lm1
summary(lm1)

#In linear regression, we want to find a straight line that fits to the majority of data. We can visualize this using geom_smooth()

#Using geom_smooth(method="lm") we can visualize the linear model
ggplot(dataExample,aes(y=yield,x=soilMoisture))+geom_point()+
  geom_smooth(method="lm",size=2) 

#This line is the same as putting the coefficients of the model into an abline
ggplot(dataExample,aes(y=yield,x=soilMoisture))+geom_point()+
  geom_smooth(method="lm")+
  geom_abline(slope=-0.02593,intercept=0.01629,col="red",size=2)

#We can also make a predicted line by putting the soilMoisture values into the a model
Predicted<- -0.02593*soilMoisture + 0.01629

#When we plot this it will also overlap
ggplot(dataExample,aes(y=yield,x=soilMoisture))+geom_point()+
  geom_smooth(method="lm")+
  geom_line(aes(y=Predicted),col="green",size=2)
 
#Residuals ##########################
#Deviation for each point from the line
#Is the difference between the yield and the predicted plant height
Resid<-yield-Predicted
head(Resid)
hist(Resid)
#Can also be taken from the model which gives almost the same values (why is it different? Do you know?)
resLinReg<-residuals(lm1)
head(resLinReg)
library(DHARMa)
plot(simulateResiduals(lm1))
#Based on the residuals we can do statistics
#Fit the model again
LinReg<-lm(yield ~ soilMoisture)
summary(LinReg) #This gives a summary of the model output including pvalues
#estimate are the coefficients, std error around the estimates t value (is something with statistics) Pr>|t| is the significance
#No significant effect of soilMoisture on 
#R squared is the amount of variation explained by the statistical model
#F value tests the overall fit of the model. which you can see clearer with the anova function
anova(LinReg) #Anova just gives the signifcance of the soilMoisture 
 

#New plot: yield as a function of ferti
#Plot shows a clear correlation
ggplot(dataExample,aes(x=ferti,y=yield))+geom_point() + geom_smooth(method="lm")
CorMod<-lm(yield ~ ferti,data=dataExample)
summary(CorMod) #The Pvalue is significant R-squared is 0.48 so 48% of the data is explained by the model.
anova(CorMod) #And the model is significant

#elevation model
boxplot(yield~elevation,dataExample) #No difference between the three treatments
ggplot(dataExample,aes(x=elevation, y=yield))+geom_boxplot()

CatMod<-lm(yield ~ elevation,dataExample) #We fit a elevation model or Anova


summary(CatMod) #(Intercept) is the mean of the High elevation. The other two are compared to the High
meansPerCat<-aggregate(yield~elevation,dataExample,mean) #Calculate the means of the three groups using aggregate (Note the same synthax as for the linear model!)
meansPerCat #Here you can see the means of the three groups
meansPerCat$yield[2]-meansPerCat$yield[1] #If we substract Medium from High we get the value in the model output
meansPerCat$yield[3]-meansPerCat$yield[1] #Same for the Low value

aov1<-aov(CatMod) #Using anova we can see the significance of the model
summary(aov1)  #Which is not significant. (This does not show the different elevations)
TukeyHSD(aov1) #a Tukey post hoc test gives the significance between the different habitat types

#Generate some significant elevation data
dataExample$plantHeight<-dataExample$yield
dataExample$plantHeight[dataExample$elevation=="High"]<-dataExample$yield[dataExample$elevation=="High"]+0.1
dataExample$plantHeight[dataExample$elevation=="Low"]<-dataExample$yield[dataExample$elevation=="Low"]+2
####################
ggplot(dataExample,aes(x=elevation, y=plantHeight))+geom_boxplot()
#Strong differences between solar park and Medium/High

CatModCor<-lm(plantHeight ~ elevation,dataExample)
summary(CatModCor) #Low differs significantly from Medium. High does not differ signifcantly from Medium


aov2<-aov(CatModCor)
summary(aov2) #elevation has asignificant effect
TukeyHSD(aov2) #Significance between Low / High+Medium 

#Multiple predictors



ggplot(dataExample,aes(x=ferti,y=plantHeight,col=elevation))+geom_point()+
  geom_smooth(method="lm") #plantHeight as functio of ferti and habitat type

lm3<-lm(plantHeight~ferti+elevation,dataExample) #Additive effects (+). Only the intercept is being used.
summary(lm3)  #Significant  positive effect of ferti. 
              #Low has a significant effect on plantHeight
              #Trend for High
aov3<-aov(lm3)
summary(aov3) #Shows significant effect of elevation and ferti


#Interactions


lm4<-lm(plantHeight~ferti*elevation,dataExample) #Interactive effects
summary(lm4) #No significant interactions: High and Low do not react differently then Medium to ferti

#Make some more significant data
dataExample$plantArea<-dataExample$plantHeight
dataExample$plantArea[dataExample$elevation=="High"]<-dataExample$soilMoisture[dataExample$elevation=="High"]-rnorm(length(dataExample$yield[dataExample$elevation=="High"]))

#Significant interaction: Low has larger plantArea. But similar reaction to soilMoisture as Medium.
#In High leaf width has a strong positive relation with soilMoisture
ggplot(dataExample,aes(x=soilMoisture,y=plantArea,col=elevation))+geom_point()+
  geom_smooth(method="lm")

lm5<-lm(plantArea~soilMoisture*elevation,dataExample)
summary(lm5) #soilMoisture in Medium has no significant effect on plantArea. 
             #Low has a significant positive effect on plantArea
             #In High soilMoisture has a significant effect on plantArea

anova(lm5) #All of the variables are significant but it is hard to tell which direction from this output so for that you have to look in to the lm output
ggplot(dataExample,aes(x=soilMoisture,y=plantArea))+geom_point()+
  geom_smooth(method="lm")

lm6<-lm(plantArea~soilMoisture,dataExample)
summary(lm6)


par(mfrow=c(2,2))
plot(lm5) #Some diagnostics plot. Will go into this more thouroughly later.

