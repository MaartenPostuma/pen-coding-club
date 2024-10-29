#This coding club has been adjusted from https://colauttilab.github.io/RIntroStats/6_GeneralizedLM.html 
#I would recommend checking this out for any other R questions you have
rm(list=ls())
library(ggplot2) # plotting library
library(dplyr) # data management
library(DHARMa) #alternative way to plot residuals


#Generate the data ######################
N<-1000
precipitation<-rnorm(N, sd=3)
location<-sample(c("Forest","Grassland"), N, replace=T)
tDat<-data.frame(precipitation=precipitation, location=location)
tDat$locationdev<-as.numeric(as.character(factor(location,levels=c("Forest","Grassland"),labels=c(0,3))))
tDat$plantHeight<- -1 + 0.3*tDat$precipitation + tDat$locationdev + rnorm(N, sd=2)
# tDat$plantHeight[99]<-max(tDat$plantHeight)*3

ggplot(tDat,aes(x=precipitation,y=plantHeight,col=location))+
  geom_point()+
  stat_smooth(method="lm")+
  theme_bw()


lm1<-lm(plantHeight~precipitation + location, data=tDat)
summary(lm1) #What does this output mean?




#What are residuals again?
#linear model assumes that residuals are normally distributed
lm1Residuals<-residuals(lm1)
hist(lm1Residuals)

# shapiro.test(lm1Residuals) #Would not recommend (biological data is rarely really normally distributed)
par(mfrow=c(2,2))
plot(lm1) #What do these plots show:


plot(simulateResiduals(lm1))

#Residuals vs fit show if there is any non-linear relationship between predictor variables
#If everything is equally spread across a horizontal line this is fine!
#Q-Q plot do the residuals follow the straightline well. There is some wiggle room here
#Scale-location, checks for heteroscadaticity. Is the size of the residuals that affect model outcome
#Residuals vs Leverages are there any points with very large residuals that affect the model fit a lot?
#For more explanation also see: https://data.library.virginia.edu/diagnostic-plots/ 


#Fitting a generalized linear model:

glm1<-glm(plantHeight~precipitation + location,family="gaussian", data=tDat)
summary(glm1) #What does this output mean?
par(mfrow=c(2,2))
plot(glm1) #What do these plots show:
plot(simulateResiduals(glm1))
# 
# tDat$Logit<-1/(1+exp(-(-1 + 0.3*tDat$plantHeight + tDat$locationdev)))
# par(mfrow=c(1,1))
# plot(Logit~precipitation,data=tDat)



tDat$plantSurvival<-rbinom(n=N,size=1,prob=tDat$Logit)
hist(tDat$plantSurvival)
ggplot(tDat,aes(x=precipitation,y=plantSurvival,col=location))+geom_point()+
  theme_bw()

lm2<-lm(plantSurvival~precipitation+location,data=tDat)
summary(lm2)

ggplot(tDat,aes(x=precipitation,y=plantSurvival,col=location))+geom_point()+
  theme_bw()+
  stat_smooth(method="lm")

par(mfrow=c(2,2))
plot(lm2) 

plot(simulateResiduals(lm2))


glm2<-glm(plantSurvival~precipitation+location,family="binomial",data=tDat)


summary(glm2)
ggplot(tDat,aes(x=precipitation,y=plantSurvival,col=location))+geom_point()+
  theme_bw()+
  stat_smooth(method="glm",method.args = c(family="binomial"))

par(mfrow=c(2,2))
plot(glm2)#
#Logit link = e^x / 1+e^x
plot(simulateResiduals(glm2))

exp(-1.09965+0.15996*precipitation)/(1+exp(-1.09965+0.15996*precipitation))


##############poisson distribution ######################

Log<-exp(-1 + 0.3*tDat$plantHeight + tDat$locationdev)
tDat$numberOfBees<-rpois(N,lambda=Log)
hist(tDat$numberOfBees)


ggplot(tDat,aes(x=precipitation,y=numberOfBees,col=location))+geom_point()+
  theme_bw()

lm3<-glm(numberOfBees~precipitation+location,data=tDat)
summary(lm3)
plot(simulateResiduals(lm3))


ggplot(tDat,aes(x=precipitation,y=numberOfBees,col=location))+geom_point()+
  theme_bw()+
  stat_smooth(method="lm")

par(mfrow=c(2,2))
plot(lm3) 

plot(simulateResiduals(lm3))

glm3<-glm(numberOfBees~precipitation+location,family="poisson",data=tDat)
plot(simulateResiduals(glm3))


summary(glm3)

ggplot(tDat,aes(x=precipitation,y=numberOfBees,col=location))+geom_point()+
  theme_bw()+
  stat_smooth(method="glm",method.args=c(family=poisson))

par(mfrow=c(2,2))
plot(glm3) 



glm3<-glm(numberOfBees~precipitation*location,family="poisson",data=tDat[tDat$numberOfBees>0,])



glm4<-glmmTMB(numberOfBees~precipitation+location,family=nbinom1,data=tDat)
summary(glm4)

plot(simulateResiduals(glm4))




#Gabriella's additions -------------------------------------------------------------------
library(lme4) #for linear mixed models
library(glmmTMB) #for generalized linear mixed models
library(car) #for vif() function
library(performance) #for check_collinearity() if using glmmTMB
library(DHARMa) #for checking residuals

BH <- read.csv("data/BH5yr_260123.csv", fileEncoding = "UTF-8-BOM") #read in csv
BH <- subset(BH,!is.na(BH$Flwr_Rich)) #removing NAs
BH$Year <- as.factor(BH$Year) #setting numeric random effect to factor variable

#make an lmm
BH$Bees_sqrt <- sqrt(BH$Bee_abun)
BH$Bees_log <- log(BH$Bee_abun+1)
lmm1 <- lmer(Bees_sqrt~Flwr_Rich+Flwr_cov_perc+(1|Site)+(1|Year),data=BH)
summary(lmm1)
car::Anova(lmm1)
vif(lmm1) #looks fine because less than 3
ranef(lmm1)
plot(simulateResiduals(lmm1)) #violates model assumptions!

#take 2
lmm2 <- lmer(Bees_log~scale(Flwr_Rich)+scale(Flwr_cov_perc)+(1|Site)+(1|Year),data=BH)
summary(lmm2)
vif(lmm2) #looks fine because less than 3
ranef(lmm2)
plot(simulateResiduals(lmm2)) #violates model assumptions!

#we could continue trying different transformations, or we can model the data with its original intended distribution

#make a glmm with count data (poisson)
glmm1 <- glmmTMB(Bee_abun~Flwr_Rich+Flwr_cov_perc+(1|Site)+(1|Year),data=BH,family="poisson")
summary(glmm1)
check_collinearity(glmm1)
ranef(glmm1)
plot(simulateResiduals(glmm1)) #still not good! but why?
testZeroInflation(glmm1) #zero inflation!!!
testDispersion(glmm1) #barely not significant. let's deal with zero inflation first

glmm2 <- glmmTMB(Bee_abun~Flwr_Rich+Flwr_cov_perc+(1|Site)+(1|Year),data=BH,family="poisson",
                 ziformula = ~Flwr_Rich+Flwr_cov_perc)
summary(glmm2)
plot(simulateResiduals(glmm2)) #still bad!
testZeroInflation(glmm2) #taken care of
testDispersion(glmm2) #not good! the model is overdispersed.

glmm3 <- glmmTMB(Bee_abun~Flwr_Rich+Flwr_cov_perc+(1|Site)+(1|Year),data=BH,family="nbinom2",
                 ziformula = ~Flwr_Rich+Flwr_cov_perc)
plot(simulateResiduals(glmm3)) #yay! overdispersion is gone. we still have heteroskedasticity.
res <- simulateResiduals(glmm3)
plotResiduals(res, form=BH$Flwr_cov_perc)
plotResiduals(res, form=BH$FlwrRich)

glmm3.1 <- update(glmm3, dispformula=~Flwr_Rich+Flwr_cov_perc)
plot(simulateResiduals(glmm3.1)) #the model still has heteroskedasticity. That means that there is something influencing the residuals that is unmodelled. In this situation, it could be for example habitat type. We will not explore this further here.

#let's pretend we have the model that we want now
summary(glmm3.1) #we see a table of coefficients and standard errors. THESE ARE ON THE LOG SCALE. WHY?

#let's graph our model output 
library(ggeffects)
ggpredict(glmm3.1,~Flwr_cov_perc,type="zi_random") %>% plot() #this is weird!! manually make your graphs if you can

predict.data <- data.frame(Flwr_cov_perc=with(BH,seq(min(Flwr_cov_perc),
                                                     max(Flwr_cov_perc),
                                                     length.out=nrow(BH)))) #make a data frame for predicting across the range of your variable of interest
predict.data$Flwr_Rich <- mean(BH$Flwr_Rich) #We are predicting the MARGINAL effect of flwr_cov, AKA the effect of flwr_cov holding flwr_rich constant
predict.data$Year <- NA #set random effects to NA
predict.data$Site <- NA

model.predictions <- predict(glmm3.1,newdata=predict.data,re.form=NULL,se.fit=T)
predictions <- as.data.frame(model.predictions$fit)
predictions$fit <- predictions$`model.predictions$fit`
predictions$se <- model.predictions$se.fit
predictions$upr <- predictions$fit+(1.96*predictions$se) #first calculate CIs BEFORE back-transforming
predictions$lwr <- predictions$fit-(1.96*predictions$se)
predictions_backtransformed <- mutate(predictions,fit=exp(fit),upr=exp(upr),lwr=exp(lwr))
predictions_backtransformed$Flwr_cov_perc <- predict.data$Flwr_cov_perc
predictions$Flwr_cov_perc <- predict.data$Flwr_cov_perc
pb <- predictions_backtransformed

ggplot()+ #plot with back-transformed predictions
 geom_ribbon(aes(x=pb$Flwr_cov_perc,ymin=pb$lwr,ymax=pb$upr),alpha=0.3)+
  geom_line(aes(x=pb$Flwr_cov_perc,y=pb$fit))+
  labs(x="Flower cover (%)",y="Bee abundance")+
  geom_point(aes(x=BH$Flwr_cov_perc,y=BH$Bee_abun),alpha=0.1)
  #ylim(0,500)

ggplot()+ #plot with untransformed predictions
  geom_ribbon(aes(x=predictions$Flwr_cov_perc,ymin=predictions$lwr,ymax=predictions$upr),alpha=0.3)+
  geom_line(aes(x=predictions$Flwr_cov_perc,y=predictions$fit))+
  labs(x="Flower cover (%)",y="Bee abundance")+
  geom_point(aes(x=BH$Flwr_cov_perc,y=BH$Bee_abun),alpha=0.1)
#----------------------------------------------------------------------------