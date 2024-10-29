rm(list=ls())

#Load data (save data sheet as csv (comma separated))
#Since excel is stupid it sometimes saves comma separated as semicolon separated....
#If that is the case run read.csv2
#you can also import directly from excel using the import data set button to the right
data<-read.csv("C:/Users/postu003/Dropbox/PhD/Onderwijs/PEB 2023/data/FloodingExperiment.csv")
data<-read.csv2("C:/Users/postu003/Dropbox/PhD/Onderwijs/PEB 2023/data/FloodingExperiment.csv")

#this is data from last years arabidopsis flooding group.
#there is an alt, population size, location,treatment, no.of.leaves, longest.leaf.length and some other columns
head(data)
data$longestLeaf<-as.numeric(data$longest.leaf.length..cm.)

#We will start by making some basic plots:
boxplot(no.of.leaves~alt,data)
boxplot(no.of.leaves~popsize,data)
boxplot(no.of.leaves~Treatment,data)
boxplot(no.of.leaves~Location,data)
boxplot(no.of.leaves~Treatment*alt*popsize,data)
boxplot(no.of.leaves~Treatment*Location,data)



#Statistics: for no.of leaves
#Basic 3 way anova:
#Similar to 
lm1<-lm(no.of.leaves~alt*popsize*Treatment,data)


hist(residuals(lm1),breaks=20)
shapiro.test(residuals(lm1)) #If significant not normally distributed...
par(mfrow=c(2,2))
plot(lm1) #Diagnostic plots

#packages that I love that streamlines the diagnostics
library(DHARMa)
plot(simulateResiduals(lm1)) #This does a less strict normality test called Kolmogorov-Smirnov test. And checks for heteroscadicity. If the lines are black it is okay!


#we can now look at the summary
summary(lm1) #Not really informative, but we can wrap this in an Anova
library(car) #We need the R package car to use the Anova function as this is type II vs type III 
summary(aov(lm1)) #This is type 1 
aov1<-Anova(lm1,type=3) #This is type 3 (differences are very small...)
aov1 #three way interaction is significant. But what does this mean...
#Basicly means: there is a population effect in response to treatment
TukeyHSD(aov(lm1))

#LONGEST LEAF
par(mfrow=c(1,1))
boxplot(longestLeaf~alt,data)
boxplot(longestLeaf~popsize,data)
boxplot(longestLeaf~Treatment,data)
boxplot(longestLeaf~Treatment*popsize*alt,data)#This looks super interesting!
boxplot(longestLeaf~Treatment*Location,data) #This is the same but with popnames
boxplot(longestLeaf~Location,data)


#Statistics: for longest leaf
#Basic 3 way anova:
#Similar to 
lm3<-lm(longestLeaf~alt*popsize*Treatment,data)


hist(residuals(lm3),breaks=20)
shapiro.test(residuals(lm3)) #If significant not normally distributed...
par(mfrow=c(2,2))
plot(lm3) #Diagnostic plots

#packages that I love that streamlines the diagnostics
library(DHARMa)
plot(simulateResiduals(lm3)) #This does a less strict normality test called Kolmogorov-Smirnov test. And checks for heteroscadicity. If the lines are black it is okay!


#we can now look at the summary
summary(lm3) #We se a clear treatment effect! And no interactions

library(car) #We need the R package car to use the Anova function as this is type II vs type III 
summary(aov(lm3)) #This is type 1 
aov1<-Anova(lm3,type=3) #This is type 3 (Big differences!)
aov1 

TukeyHSD(aov(lm3))


data$trichomeBin<-as.numeric(factor(data$Trichomes,levels=c("N","Y")))-1



boxplot(trichomeBin~alt,data) #Very strong diffrences
boxplot(trichomeBin~popsize,data)
boxplot(trichomeBin~Treatment,data)
boxplot(trichomeBin~Treatment*alt*popsize,data)#This looks super interesting!
boxplot(trichomeBin~Treatment*Location,data) #This is the same but with popnames
boxplot(trichomeBin~Location,data)

aggregate(trichomeBin~alt*popsize*Treatment,data,)

#Statistics: for longest leaf
#Basic 3 way anova:
#Similar to 
lm4<-lm(trichomeBin~alt*popsize*Treatment,data)


hist(residuals(lm4),breaks=20)
shapiro.test(residuals(lm4)) #If significant not normally distributed...
par(mfrow=c(2,2))
plot(lm4) #Diagnostic plots

#packages that I love that streamlines the diagnostics
library(DHARMa)
plot(simulateResiduals(lm4)) #This does a less strict normality test called Kolmogorov-Smirnov test. And checks for heteroscadicity. If the lines are black it is okay!

#Similar to 
glm4<-glm(trichomeBin~alt*popsize*Treatment,data,family="binomial")


hist(residuals(glm4),breaks=20)
shapiro.test(residuals(glm4)) #If significant not normally distributed...
par(mfrow=c(2,2))
plot(glm4) #Diagnostic plots

#packages that I love that streamlines the diagnostics
library(DHARMa)
plot(simulateResiduals(glm4)) #This does a less strict normality test called Kolmogorov-Smirnov test. And checks for heteroscadicity. If the lines are black it is okay! There is one red line but we can't really fix it much more. 


#we can now look at the summary
summary(glm4)
library(car) #We need the R package car to use the Anova function as this is type II vs type III 
summary(aov(glm4)) #This is type 1 
aov1<-Anova(glm4,type=3) 
aov1 
TukeyHSD(aov(lm4))
                             