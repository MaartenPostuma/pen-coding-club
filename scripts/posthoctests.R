#post hoc tests

#see https://cran.r-project.org/web/packages/emmeans/vignettes/comparisons.html
#and https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html 
# and in general all the vignette topics from emmeans

#this script is largely copied from the emmeans vignettes.

#contrasts in emmeans are largely comparable to those in glht, but I find emmeans much nicer to use

library(emmeans)
library(ggplot2)

#when do we do post hoc tests? Post hoc means "after this", meaning we have already done an analysis first.
# in this case, this almost always is a (g)lm(m)

#1. BASIC PAIRWISE COMPARISONS, CATEGORICAL VARIABLE, NO INTERACTIONS

#let's make a model to start

pigs <- as.data.frame(pigs) 
pigs
#included emmeans dataset, where both "source" and "percent" are categorical variables.

#basic model with two categorical variables, no interactions. 
#Percent is coded as a factor because it's numerical in the data

lm1 <- lm(log(conc)~source+factor(percent),data=pigs)

#let's pretend this is the model that we want based on our hypotheses and our data, and we have checked all the model assumptions

#now we can proceed with checking the results of the model
summary(lm1)
#everything is significant. Let's check the overall effect of these variables, i.e. "F-test" or "omnibus test"
anova(lm1) #only use this command for lm!

#both predictors are significant. Now we want to know which levels of each factor are different from each other.

#let's check the variable "source"
emmeans(lm1,pairwise~source) #indicates pairwise contrasts, between all levels of source
aggregate(log(conc)~source,data=pigs,FUN=mean) #raw means are different - why?

#we are given the emmeans (Estimated Marginal Means) for each factor level, their 95% CIs, and all contrasts
#fish is significantly different from soy and from skim, but soy is not significantly different from skim

#we see that the tukey p-value adjustment has already been applied for us
#if you want to name another type of p-value adjustment, you use the "adjust" argument
#for example, for ultra-conservative bonferroni corrections:
emmeans(lm1,pairwise~source,adjust="bonferroni")


#how do we graph this?

#first, extract the emmeans (NOT the raw means from the data!!)
emms <- emmeans(lm1,pairwise~source)
emms <- as.data.frame(emms$emmeans)
emms$label <- c("a","b","b") #annotation to show which means are different from each other

#I will plot the back-transformed means and CIs, because they are on the log scale.

ggplot(emms,aes(source,exp(emmean)))+
  geom_bar(stat="identity",aes(fill=source))+
  geom_errorbar(aes(source,ymin=exp(lower.CL),ymax=exp(upper.CL)),width=0.5)+
  labs(y="Concentration")+
  geom_text(aes(x=source,y=exp(upper.CL)+3,label=label))+
  theme_classic()

#2. INTERACTIONS: FACTOR-BY-FACTOR (only categorical variables, 2- and 3-way interactions)

#let's make a model with an interaction effect

noise <- as.data.frame(auto.noise)
lm2 <- lm(noise/10~size*type*side,data=noise)

#let's pretend that this is the model we want and we have checked all model assumptions.
summary(lm2)
anova(lm2) #omnibus F-test for lm, lm only!!
#significant three-way interaction, some sig 2-way interactions

#let's visualize the 3-way interaction. How you break up the plot should be informed by your knowledge of the data
emmip(lm2, type~size |side) #built in plotting function from emmeans
#we are looking at the effects of size on noise levels, separated across type and left or right side.

#DEPENDING ON OUR RESEARCH QUESTIONS, we might be interested in all possible comparisons between groups of size:type:side.
emmeans(lm2,pairwise~size*type*side)
# here we have a crazy number of contrasts, and a warning saying we maybe generated more contrasts than we really wanted
# the tukey adjustment that has been applied is for 12 estimates - this is related to the number of means we have, not the number of contrasts

#we are probably actually only interested in some of these contrasts
# for example, what we visualized in the previous plot
emmeans(lm2,pairwise~size|side*type)
#now we have p-value adjustment only based on 3 means.

#3. INTERACTIONS: FACTOR BY CONTINUOUS 

#model
fiber <- as.data.frame(fiber)
lm3 <- lm(strength~diameter*machine,fiber)
summary(lm3)
anova(lm3)

#lets check if the SLOPES of the relationship between diameter and strength are different between machines
emtrends(lm3,pairwise~machine,var='diameter')
#no significant differences

emmip(lm3,machine~diameter,cov.reduce=range)
#this is the same thing as manually predicting at each level of machine, and then plotting all three lines together.

#4. CUSTOM CONTRASTS IN EMMEANS
#see https://aosmith.rbind.io/2019/04/15/custom-contrasts-emmeans/

#maybe you have many treatment levels, and you don't need to compare them all, just with one control group.

dat = structure(list(sub.rate = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 
                                            2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 
                                            5L, 5L), .Label = c("A.1", "A.2", "B.1", "B.2", "control"), class = "factor"), 
                     resp = c(5.5, 4.9, 6.1, 3.6, 6.1, 3.5, 3, 4.1, 5, 4.6, 7.3, 
                              5.6, 4.8, 7.2, 6.2, 4.3, 6.6, 6.5, 5.5, 7.1, 5.4, 6.7, 6.8, 
                              8.5, 6.1)), row.names = c(NA, -25L), class = "data.frame")

#let's make a simple model to compare the treatment with the response

lm5 <- lm(resp~sub.rate, data=dat)

#here we can use emmeans built-in contrasts to compare each treatment with the control

emmeans(lm5, trt.vs.ctrlk~sub.rate)
#note that a different p-value adjustment has been chosen by default here!

#cool, now we know how each group compares to the control. But what about other contrasts?

#first, extract the estimated marginal means (EMMeans) for each group and save them as an object.

rate.emm <- emmeans(lm5, ~sub.rate)

#To tell emmeans the position of each group, we need to make a vector for each group
rate.emm
#we can see that first comes A.1, then A.2, etc and control is last.

#we specify the position in the vector, we use a 1, and for others a 0
A.1 = c(1,0,0,0,0)
A.2 = c(0,1,0,0,0)
B.1 = c(0,0,1,0,0)
B.2 = c(0,0,0,1,0)
control = c(0,0,0,0,1)

#now let's make a custom contrast using our emmGrid object.
contrast(rate.emm, method=list("A.1-B.1"=A.1-B.1))

#now we can see that we have the contrast between A.1 and B.1.
#Let's do a few more

contrast(rate.emm, method=list("A.1-B.1"=A.1-B.1,
                               "A.2-B.2"=A.2-B.2,
                               "B.1-B.2"=B.1-B.2,
                               "A.1-A.2"=A.1-A.2),adjust="Tukey")
#I chose the wrong adjustment method, so emmeans automatically changed it for me.
#If we didn't specify an adjustment though, none would be made, because these are custom contrasts.
#now we can see our four contrasts, with p-value adjustment.
#this is fewer contrasts than if we simply did all pairwise contrasts. So we are able to have less conservative p-value adjustment.