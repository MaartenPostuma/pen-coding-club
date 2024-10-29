rm(list=ls())
#Shamelessly stolen from https://ourcodingclub.github.io/tutorials/mixed-models/ because I like dragons and the data structure is similar to what you will find in your own data sets
#But see also https://colauttilab.github.io/RIntroStats/8_MixedModels.html#Fixed_or_Random which is slightly less ecology related
library(ggplot2)  # load ggplot2
theme_set(theme_classic()) #Set the theme of ggplot to something non grey background cause i like it better that way
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                       "#0072B2", "#D55E00", "#CC79A7",  "#F0E442") #Use my colour blind friendly colourscheme


load("data/dragons.RData")
head(dragons) #Dataset in which they tested dragon intelligence from dragons of different sizes and mountian ranges.


hist(dragons$testScore) 
basic.lm <- lm(testScore ~ bodyLength, data = dragons) #Fit a simple model
summary(basic.lm) #Check the results, what do they mean?



ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm") #Plot the results

par(mfrow=c(2,2))
plot(basic.lm) #Plot the diagnostics #How do they look?

#We sampled the dragons in 8 different mountain ranges is there an effect of mountain range on dragons intelligence?
par(mfrow=c(1,1))
ggplot(dragons,aes(x=mountainRange,y=testScore,fill=mountainRange))+geom_boxplot()+
  scale_fill_manual("",values=colorBlindBlack8)

ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange)) +
    geom_point(size = 2) +
    scale_color_manual("",values=colorBlindBlack8)


#How can we solve this?

#Run multiple analysis

ggplot(dragons, aes(x = bodyLength, y = testScore)) +
  geom_point(size = 2) +
  scale_color_manual("",values=colorBlindBlack8)+
  facet_wrap(.~mountainRange)

#We can run an lm for each mountain range seperately however we also have 3 sites per mountain range
ggplot(dragons, aes(x = bodyLength, y = testScore,col=site)) +
  geom_point(size = 2) +
  scale_color_manual("",values=colorBlindBlack8)+
  facet_wrap(.~mountainRange)


#If we fit models we get a slope and intercept for each site and mountain range
#This will be 48 parameters that need to be estimate. If we do this we also increase the chance for false positives.
#More tests lead to higher chance of having pvalues <0.05

#We can fit a model including the mountain range:
mountain.lm <- lm(testScore ~ bodyLength + mountainRange, data = dragons)
summary(mountain.lm)
#What does this output mean? 
#Are we interested in this answer?


######################
#What is a random effect? 
#Explanatory: variables we expect to have an effect on the response variable
#random effects: grouping factors for which we try to control. Are always categorical.
#We do not want to find the differences between different mountain ranges but control for the variation between
#It really depends on the question you have! (As always...)

#Some more notes:
#"A random effect should have atleast 5 levels". If we wanted to include for the effect of dragon's sex on intelligence we would fit sex as a fixed effect. #You can then take substract the intercepts and divide by 2. 
#The goal is to make better estimates. What is considered "random" noise.



#Fitting random effect models:
library(lme4) #(There is also nlme as a package but I do not use this)
mixed.lmer <- lmer(testScore ~ bodyLength + (1|mountainRange), data = dragons)
summary(mixed.lmer)
#What does this summary mean.
#Random effects:
#The amount of variance explained by mountainRange
339.7/(339.7+223.8) #Mountain range explains 60% of the variance

#Fixed effects: 
#Very similar to the normal lm output
#However, there are no pvalues???
#This is because the authors of the package do not believe in pvalues!


#Check diagnostics

plot(mixed.lmer) #There do not seem to be any patterns here

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

#What does the (1|mountainRange) mean?
#How I see it that it adjusts the intercept (1) by adjusting for the mountain range


#Nested random effects:
#If your model contains sampling stratification or different levels of classification.
#This sounds complicated but our dragons have multiple sites that only occur in each mountain range
#i.e. site a in bavaria and site b in bavaria etc. #Think of russian dolls.
#If we want to include these 


mixed.lmer2 <- lmer(testScore ~ bodyLength +(1|mountainRange/site), data = dragons)
summary(mixed.lmer2)

ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    scale_colour_manual("",values=colorBlindBlack8)+
    geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1)# adding predicted line from mixed model 

#All of the slopes are still the same. #Do we expect these to differ between the different mountainranges?
mixed.ranslope <- lmer(testScore ~ bodyLength + (1 + bodyLength|mountainRange/site), data = dragons) 
summary(mixed.ranslope)
#NOTE the model starts to throw errors. This "Usually" indicates that there is overfitting
#Keep in mind that you will need about 10 times the amount of data per sample than parameters
#Things that may help is scaling your data (However this may lead to harder to interpret your results)
#Or decreasing the amount of parameters in your random/fixed effects


ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
    facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
    geom_point(alpha = 0.5) +
    scale_colour_manual("",values=colorBlindBlack8)+
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1)  # adding predicted line from mixed model 

  
  
#Bonus##### So this does not work if you use the lmerTest package we use to calc pvalues.... so this is a weird order
#But it gives a nice table for your paper! (It can export as text or latex or html) #Might recommend!
library(stargazer)

stargazer(mixed.lmer, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


stargazer(basic.lm, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

  
#HOW TO GET THE PVALUES
  
#eventhough the authors do not like pvalues we do!
#There are multiple ways to do this and staticians have complicated discussions about which are best
  
library(lmerTest)  
mixed.lmer <- lmer(testScore ~ bodyLength +(1|mountainRange),data = dragons)
summary(mixed.lmer)


mixed.lmer2 <- lmer(testScore ~ bodyLength +(1|mountainRange/site), data = dragons)
summary(mixed.lmer2)

#You can also compare two models of which one includes your variable of choice

full.lmer <- lmer(testScore ~ bodyLength + (1|mountainRange/site), 
                  data = dragons, REML = FALSE)
reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange/site), 
                     data = dragons, REML = FALSE)
anova(reduced.lmer, full.lmer)  # the two models are not significantly different

#I.e. there is no significant effect of body size on dragon intelligence!

