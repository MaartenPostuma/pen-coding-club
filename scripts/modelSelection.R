library(tidyverse)
library(ggplot2)
rm(list=ls())
#Adjusted from:  https://colauttilab.github.io/RIntroStats/5_ModelSelection.html and https://colauttilab.github.io/RIntroStats/4_AdvancedLM.html
setwd("C:/Users/bisho002/Downloads/pen-coding-club-main/pen-coding-club-main")

fitnessData<-read.csv("data/fitnessEnvData.csv")
head(fitnessData)
#Plant performance data with many environmental variables collected using soil analysis.
#Number of inflorescences.plant, inflorescence.length..cm. emerg_per (number of seeds germinating) aver_w (seed weight) 

#How to run an analysis and how to choose your models
#So far we have considered the following way of analysis: 

plot(Inflorescence.length..cm.~pH,fitnessData)

lm1<-lm(Inflorescence.length..cm.~pH,data=fitnessData)
par(mfrow=c(2,2))
plot(lm1)
summary(lm1) #Model is significant! Hooray!

ggplot2::ggplot(fitnessData, aes(x=pH, y=Inflorescence.length..cm.))+
         geom_point()+
         geom_smooth(method="lm") #can we trust this relationship?

#However we have a lot of data from pH to Fe concentrations to population size
#We can fit many indepent models see if each of them is signifcant. However this can lead to type I error (False positives)
#If p <0.05 there is a 1 in 20 chance of it being a coincedence. If you then fit 20 models some will likely be significant.
set.seed(3142023)
simulatedPvalues<-runif(n=1000)
hist(simulatedPvalues)
sum(simulatedPvalues<0.05)
#Multiple testing correction
#False discovery rate: calculates the chance of each value to be a false discovery
adjustedSimValues<-p.adjust(simulatedPvalues,method = "fdr")
hist(adjustedSimValues)

#Bonferroni is more strict! 
adjustedSimValues<-p.adjust(simulatedPvalues,method = "bonferroni")
hist(adjustedSimValues) #Everything is a 1.
#Nothing is significant anymore...



#You could do this but we could also take a slightly different approach
#Which models fits my data best?
#Include population size (says something about inbreeding)
#Important No NAs!!!! If there are NAs in your explantory variables they can lead to differences in sample size. This renders the use of these model fit values unuseable.

lm1<-lm(Inflorescence.length..cm.~pH,data=fitnessData)

lm2<-lm(Inflorescence.length..cm.~pH+popSize,data=fitnessData)
summary(lm2)
par(mfrow=c(2,2))
plot(lm2)

#Also include Fe in the soil 
lm3<-lm(Inflorescence.length..cm.~pH+popSize+Fe,data=fitnessData)
summary(lm3)
par(mfrow=c(2,2))
plot(lm3)


#One of the things you can look at is the r-squared
summary(lm1)
summary(lm2)
summary(lm3)
#Including more variables improves the Rsquared!
anova(lm2)
#We can also use an Anova to see if including a variable improves the model more than chance:
anova(lm1,lm2)#Including population size improves the model!
anova(lm2,lm3)#Including Fe in the soil does not improve the model

#We can also use AIC (Aikakes information coefficient)
#Number of parameters in the model vs the likelihood.
#How well does the model fit with as low complexity as possible
#The lower the value the better. Models with a difference <2 are basicly the same fit
AIC(lm1) 
AIC(lm2)#This improve the model somewhat!
AIC(lm3)#This does not improve the model although it is basicly the same fit

#gabriella's additions##################------------------------------------------------
library(lme4) #for linear mixed models
library(car) #for vif() and Anova()
library(DHARMa) #for checking residuals
library(MuMIn) #for backwards selection, aicc, model averaging

fitnessData$InfL <- fitnessData$Inflorescence.length..cm. #renaming that annoying variable
fitnessData$Fe.scaled <- scale(fitnessData$Fe)

m1 <- lmer(InfL~scale(Fe)+scale(popSize)+scale(organic.matter)+scale(NO3)+(1|pop), data=fitnessData, REML=F)
vif(m1)
plot(simulateResiduals(m1)) #heteroskedasticity!! can indicate unaccounted for variation in residuals, for example due to an unmodelled variable, space, or time. Can also indicate unequal variances across levels of predictor
#variables that are already in the model, which needs to be explicitly modelled (possible through nlme or glmmTMB packages). we will not cover this for now

#we have four different predictor variables. How do we know if we should keep them all?

#option 1 : evaluate the whole model, leave all variables in, because they are important to answering your research questions. They might also be important to leave in if they are a covariate that should be controlled for, 
#even if they themselves do not significantly affect the response variable. This choice should only be made if you have good argument to do so in the context of your research.

#for mixed models, whether lmm or glmm, use Anova() from car to calculate an F- or chisq- table:
Anova(m1) #Chi-sq test, largely similar to dropping one at a time but slightly different.
anova(m1) #F-test, no p-values because of the lme4 package. less suitable for mixed models compared to chisq test

#another option for mixed models is LLRTs (dropping one variable at a time):
LLRTtable <- drop1(m1, test="Chisq") #makes a table summarizing what would happen if you drop one single variable, for each variable in the model
#we can also do this manually
m2 <- update(m1, ~.-scale(Fe))
anova(m1,m2) #this anova tests the difference between these two models with an LLRT (equivalent to a chisq test). You can see that the results are the same as what we calculated in the table. You can do this one by one if you
#would like to record the significance of each variable one by one.

#option 2: backwards selection. dropping variables one by one until you have a model only with significant variables.
#we know from the LLRT table above that we should first drop Fe because it is the least significant. We already have this model represented by m2.
table2 <- drop1(m2, test="Chisq") #tells us we should next drop NO3
m3 <- update(m2, ~.-scale(NO3))
table3 <- drop1(m3, test="Chisq") #now we should drop popSize
m4 <- update(m3, ~.-scale(popSize))
table4 <- drop1(m4, test="Chisq") #actually now the p-value is no longer significant. this demonstrates the weirdness of backwards selection.

AIC(m1, m2, m3, m4)
AICc(m1, m2, m3, m4) #more conservative estimate for small sample sizes

#option 3: model averaging
options(na.action=na.fail)
models <- dredge(m1) #all possible models
modelset <- get.models(models, subset = delta < 2) #subsetting models within 2 AICc of best model
modeltable <- model.sel(modelset) #table of the selected models
avgmod <- model.avg(modelset) #averaging the selected models
avgmodsumm <- summary(avgmod) #storing the average model summary
coeftable <- as.data.frame(avgmodsumm$coefmat.full) #table of the "full" coefficients, which means that variables were assigned a coefficient of zero if they are not present in the model.

############################---------------------------------------------------------

##################################


#Dealing with colinearity
leafLength<-rnorm(1000)
leafWidth<-rnorm(1000)
leafCircumfence<-leafLength+leafWidth*2
leafArea<-10+0.2*leafLength+1.2*leafCircumfence+rnorm(100)

plot(leafArea~leafLength)
plot(leafArea~leafWidth)
plot(leafArea~leafCircumfence)


leafMod1<-lm(leafArea ~ leafLength + leafWidth + leafCircumfence)
summary(leafMod1) #NA in the model output! This is always a problem!!! 
#The cicrumfence does not add anything as it is leaf width + leaf length *2



leafLength<-leafWidth+leafCircumfence+rnorm(1000,sd=0.001)
leafArea<-10+0.2*leafWidth+1.2*leafCircumfence+rnorm(100)
par(mfrow=c(2,2))
plot(leafArea~leafLength)
plot(leafArea~leafWidth)
plot(leafArea~leafCircumfence)

leafMod2<-lm(leafArea ~ leafLength + leafWidth + leafCircumfence)
summary(leafMod2)
cor(leafLength,leafWidth)
cor(leafCircumfence,leafLength)
plot(leafMod2)

leafMod3<-lm(leafArea ~ leafLength)
leafMod4<-lm(leafArea ~ leafLength + leafWidth)

AIC(leafMod3)
AIC(leafMod4)

vif(leafMod2)

#Use multivariate analysis!!! 28th of march
