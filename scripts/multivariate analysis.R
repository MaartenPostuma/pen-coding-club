# multivariate analysis: considering multiple variables at the same time.
# options: easier but less options: Canoco (spss apparently?), more difficult but more options: R, other programming languages

# packages needed: 'visreg','gridGraphics', 'ggplot2', 'vegan', 'dplyr', 'MASS', 'car', 'pls','dendextend', 'factoextra','NbClust', 'class', 'corrplot'. You can install them with install.packages('...')


# Why multivariate analysis? Multivariate advantage -----------------------

# if you dont understand something, simulate data and try what happens!
# create two groups (group a and group b) which can be distinguished based on variables x and y 
xa=rnorm(1000) #create 1000 random numbers with standard deviation 1 and mean 0
hist(xa) # histogram of xa, should be normally distributed
ya=0.8*xa + rnorm(1000) # create ya which is somewhat related to xa.
xb=xa
yb=0.8*xb + rnorm(1000)+2.5
group=c(rep(0,1000),rep(1,1000))
DAT=data.frame(X=c(xa,xb),Y=c(ya,yb),Group=group)

Rx=range(c(xa,xb))
Ry=range(c(ya,yb))

par(mfrow=c(1,3))
plot(xa,rep(0,length(xa))-0.1,pch=20,col='red',xlim=c(Rx[1],Rx[2]),ylim=c(-2,2),xlab='x',ylab='',yaxt='n')
points(xb,rep(0,length(xa))+0.1,pch=20,col='blue')
title(paste('Group~X','PredAcc=','-%'))# no accuracy because xa=xb

plot(ya,rep(0,length(ya))-0.1,pch=20,col='red',xlim=c(Ry[1],Ry[2]),ylim=c(-2,2),xlab='y',ylab='',yaxt='n')
points(yb,rep(0,length(ya))+0.1,pch=20,col='blue')
glm.fit1=glm(Group~Y, data=DAT, family=binomial) # see '# to get an idea of the model.' for visualisation
P=predict(glm.fit1,DAT,type='response')
P[P<0.5]=0
P[P>0]=1
title(paste('Group~Y','PredAcc=',100-sum(abs(group-P))/10,'%'))

plot(xa,ya,pch=20,col='red',xlim=c(Rx[1],Rx[2]),ylim=c(Ry[1],Ry[2]),xlab='x',ylab='y')
points(xb,yb,pch=20,col='blue')
glm.fit2=glm(Group~Y+X, data=DAT, family=binomial)
P=predict(glm.fit2,DAT,type='response')
P[P<0.5]=0
P[P>0]=1
title(paste('Group~X+Y','PredAcc=',100-sum(abs(group-P))/10,'%'))

# to get an idea of the model. 
par(mfrow=c(1,1))
library(visreg)
visreg(glm.fit1,'Y',scale='response')


# Why multivariate analysis? Explore data with many variables, PCA -----------------------

# introducing an example dataset
# four characteristics of three iris species are measured.
# you cant explore all variables at the same time in a 4/5-D space because you are not Jezus.
# what you can do is plot all pairwise combinations of these variables, this is often done to explore data.
par(mfrow=c(1,1))
pairs(iris,col=iris$Species) # notice here that there seems to be a multivariate advantage too. Petal width and petal length at the same time appear to better predict the species than petal width and petal length separataly

# This is however not the greatest, because you need to check many pairs, and sometimes more than two variables at the same time are responsible for clustering.
# you cant plot four dimensions at the same time. What you can do is to try to describe the most 'important' information in the data only in less dimensions.
# PCA is such a method. It can reduce the dimensionality of a dataset

# explaining PCA

# plotting the raw data
xa=scale(xa) # removing mean of variable, then dividing by the standard deviation. Advantage: every variable with same range, no difficulties when using PCA. Here a bit of an overkill, because mean xa should already be 0 and sd should already be one, but you get the drill.
ya=scale(ya)
plot(xa,ya,asp = 1,pch=20)

# this will become the first axis of PCA
abline(0,1,col='red',lwd=3) # PCA finds the direction in which the covariance is the highest. This will be the first axis of PCA (PC1)


# this will become the second axis of PCA
abline(0,-1,col='blue',lwd=1)# PCA then finds the direction, orthogonal to PC1, in which the covariance is the highest. This will be the second axis of PCA (PC2). Next PC's are found in the same way.

abline(h=0,lty=2,col='gray')
abline(v=0,lty=2,col='gray')
text(0,3.2,'ya=high',col='gray');text(0,-3.2,'ya=low',col='gray')
text(3.2,0,'xa=high',col='gray');text(-3.2,0,'xa=low',col='gray')

# rotating the plot the data the same way PCA does
library(gridGraphics)
fun_grob <- function(){                        # Create user-defined function
  grid.echo()
  grid.grab()
}
fun_grob_out <- fun_grob()                     # Store grob output
grid.newpage()                                 # Create new plot page
pushViewport(viewport(width = 0.6,             # Create grid viewport
                      height = 0.6,
                      angle = -45))
grid.draw(fun_grob_out)                        # Draw rotated plot

# second visualisation: https://i.stack.imgur.com/Q7HIP.gif

# wrong scaling:
xa2=scale(xa) -10
ya2=scale(ya) +10
plot(xa2,ya2,asp = 1,pch=20,xlim=c(-15,15),ylim=c(-15,15))
abline(h=0,lty=2);abline(v=0,lty=2)
abline(0,-1,col='red',lwd=3)
mod0 <- my_pca <- prcomp(data.frame(xa2,ya2), scale = F,center = F, retx = T) # can go wrong here.

# PCA
# PCA can horizontally or vertically flip! That is not a problem, but can be confusing.
# Later on in this script we will introduce DCA which is more variable for non-linear unimodal data.
library(vegan)
mod0 <- rda(data.frame(xa,ya),scale=T) # in vegan (rda is from the vegan package) you can do PCA with the rda function, which is confusing. RDA without constraints = PCA.

# the data is quite well explained by the first axis only. You could reduce the data by only using the first PCA axis.
eigenvalues=eigenvals(mod0)
proportionofvairance_explained=eigenvalues/sum(eigenvalues)
proportionofvairance_explained_nice=round(proportionofvairance_explained*1000)/10
barplot(as.numeric(proportionofvairance_explained),ylim=c(0,1),xlab='PC',ylab='Proportion of variance explained') 
axis(1,1:2,1:2)

# default plot can be horrible, but makes sense
bp=biplot(mod0, display = c("sites", "species"),type=c('text','points'),scaling ='sites') # when scaling='sites' is chosen, pca is exactly equal to the rotated original data (see see multivariate analysis_pcaplot vs rotated plot.png). This option is most suitable when you want your pca plot to focus on relations (similarities, dissimilarities) between samples. This is a grand wizard (I asked him) of pca approved explanation, because I was not 100% sure myself.

# bp=biplot(mod0, display = c("sites", "species"),type=c('text','points')) # this is however the default pca plot option of R. there is no direct rotational relationship anymore with the original data. This option is more suitable when you want to focus on similarities/ dissimilarities between variables (in our case variables are correlated and they thus appear close to each other and with an angle < 90 degrees in the plot). If the eigenvalues are similar for the first axes, this plot will be similar to the previous one. In that case both the relations between variables and between samples can be explored with either plot.

# bp=biplot(mod0, display = c("sites", "species"),type=c('text','points'),scaling=3) # this is a third option, kind of a mix between the two previously mentioned options for those who cannot choose.


# fixing the horrible biplot to avoid nightmares
PCA_scores=data.frame(bp$sites) # scores (or site scores) relate to the rows in the original data
PCA_loadings=data.frame(bp$species)# loadings (or species scores) relate to the colomns in the original data
# different groups of people have different names for scores and loadings. When you dont get it, complain to your computer screen, it feels better afterwards.

# rescaling loadings with a constant, which can be done without consequence.
Scorerange=apply(PCA_scores,2,range)
Loadingrange=apply(PCA_loadings,2,range)
PCA_loadings=PCA_loadings/(max(Loadingrange/Scorerange)*1.2) # downscaling the loadings

# add some additional info for plot
PCA_loadings$species=row.names(PCA_loadings)
PCA_loadings$Xbegin=rep(0,length(PCA_loadings$PC1))# defining where the arrows should start (which is at 0,0)
PCA_loadings$Ybegin=rep(0,length(PCA_loadings$PC1))# defining where the arrows should start (which is at 0,0)

library(ggplot2)
library(ggrepel)
ggplot()+
  geom_point(data=PCA_scores, aes(x=PC1, y=PC2))+
  geom_segment(data=PCA_loadings,aes(x=Xbegin,y=Ybegin,xend =PC1 , yend = PC2),arrow = arrow(length = unit(0.05, "npc")),size=2,color='gray')+ # arrows
  geom_text_repel(data=PCA_loadings,aes(x=PC1,y=PC2,  label = species),size=10,color='gray')+ # non-overlapping text
  theme_bw() +  # no gray background in plot
  xlab(paste('PC1 (explained variance=',proportionofvairance_explained_nice[1],'%)',sep=''))+
  ylab(paste('PC2 (explained variance=',proportionofvairance_explained_nice[2],'%)',sep=''))+
  theme(legend.position="none") + # no legend
  theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank())+ # no grid in plot
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") # x an y axis on equal scale. Important for interpretation of the plot!

# see: multivariate analysis_pcaplot vs rotated plot.png for proof that pca result is (apart from a horizontal and or vertical flip in some cases) 100% equal to rotated plot if scaling='sites' was chosen.


# #

# PCA iris data

mod0 <- rda(iris[,-5],scale=T) # in vegan you can do PCA with the rda function, which is confusing, because the method rda is something else than PCA.

# the iris data is quite well explained by the first two PCA axes, so we can reduce the four dimensional data to two dimensions
eigenvalues=eigenvals(mod0)
proportionofvairance_explained=eigenvalues/sum(eigenvalues)
proportionofvairance_explained_nice=round(proportionofvairance_explained*1000)/10
barplot(as.numeric(proportionofvairance_explained),ylim=c(0,1),xlab='PC',ylab='Proportion of variance explained') 
axis(1,1:4,1:4)

# horrible default plot
bp=biplot(mod0, display = c("sites", "species"),type=c('text','points'),scaling ='sites')
# bp=biplot(mod0, display = c("sites", "species"),type=c('text','points')) # even though eigenvalues for the first two axes are different, the plot does not seem to be very different from the other one.

# therefore we can interpret make both interpretations about the measurements and about the variables from the same plot
# these are the interpretations:
# there is one group of measured Irises that is different from the rest. This is the species I. setosa as you will see in the next plot. virginica and versicolor are more similar, but as you will see in the next plot, also slightly different.
# above the setosa group, there is a point that is separate from the group. This may be an outlier (incorrect measurement, wrong species, hybrid, ....).
# setosa differs from the other two species in petal length, petal width and sepal length. They are longer/wider in the other species.
# Petal length and width are strongly correlated, sepal length is also somewhat correlated to petal length and width. (better interpretation with bp=biplot(mod0, display = c("sites", "species"),type=c('text','points')))
# sepal width and sepal length are more variable within species than between.
# PCA gives an overview, this overview can be very useful for interpretation, and the multivariate view can uncover clustering in the data that would not be found with univariate methods. However, when you are interested in the exact relations between variables or the differences between species when it comes to one specific variable, making an univariate plot is better, because the first two axes explain less than 100% of this variable.

PCA_scores=data.frame(bp$sites)
Scorerange=apply(PCA_scores,2,range)

PCA_scores$species=as.factor(iris[,5])
PCA_loadings=data.frame(bp$species)

Loadingrange=apply(PCA_loadings,2,range)
PCA_loadings=PCA_loadings/(max(Loadingrange/Scorerange)*1.2)
PCA_loadings$species=row.names(PCA_loadings)
PCA_loadings$Xbegin=rep(0,length(PCA_loadings$PC1))
PCA_loadings$Ybegin=rep(0,length(PCA_loadings$PC1))
# option to artifically create outliers
# PCA_scores[1,1:2]=c(1,1)
# PCA_scores[2,1:2]=c(1.2,1.2)

library(ggplot2)
library(ggrepel)
install.packages("ggbiplot")
ggp=ggplot()+
  geom_point(data=PCA_scores, aes(x=PC1, y=PC2,color=species))+
  geom_segment(data=PCA_loadings,aes(x=Xbegin,y=Ybegin,xend =PC1 , yend = PC2),arrow = arrow(length = unit(0.05, "npc")),size=1,color='gray')+
  geom_text_repel(data=PCA_loadings,aes(x=PC1,y=PC2,  label = species),size=5,color='black')+
  xlab(paste('PC1 (explained variance=',proportionofvairance_explained_nice[1],'%)',sep=''))+
  ylab(paste('PC2 (explained variance=',proportionofvairance_explained_nice[2],'%)',sep=''))+
  theme_bw() +  
  theme(legend.position="none") +
  theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank())+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")+
  theme(legend.position="right")
ggp

# add covariance elipse per species. Default (type='t') assumes a multivariate t-distribution and is relatively good at dealing with outliers
ggpelipse=ggp+
  stat_ellipse(data=PCA_scores, aes(x=PC1, y=PC2,color=species))
ggpelipse

library(dplyr)

# second option
# Find the minimum convex hull of the points being plotted, sensitive to outliers, better with non-normally distributed data or few data-points
hull <- PCA_scores %>%
  group_by(species) %>%
  slice(chull(PC1, PC2)) # I dont understand these things %>% either.

ggpconvexhull=ggp+  geom_polygon(data = hull,aes(x=PC1, y=PC2, colour=species, alpha = 0.2), alpha = 0)
ggpconvexhull

# see which ones are 'outliers'

ggpoutlier=ggp+  
  stat_ellipse(data=PCA_scores,type='t',level=0.99, aes(x=PC1, y=PC2,color=species))+
  geom_text(data=PCA_scores, aes(x=PC1, y=PC2,label=1:150))
ggpoutlier

# other option to plot robust covariance
# (different robust option)
# (library(robust) ; robust_covarciance=covRob(PCA_scores[PCA_scores$species=='setosa',1:2]))

library(MASS)
library(car)

COL=c('red','green','blue')
Species=unique(PCA_scores$species)
ggp_robust=ggp

for(i in 1:3){
  robust_covarciance2=cov.rob(PCA_scores[PCA_scores$species==Species[i],1:2])
  E=ellipse(as.numeric(robust_covarciance2$center),as.matrix(robust_covarciance2$cov) ,2.58,add=F) # automatically plots, which is annoying dont know how to prevent it from plotting.
  Edf=as.data.frame(E)
  ggp_robust=ggp_robust+ geom_path(data=Edf, aes(x=x, y=y),color=COL[i])
}
ggp_robust+geom_text(data=PCA_scores, aes(x=PC1, y=PC2,label=1:150))

# conclusion without artificial outliers: no radical outliers. iris 42 is a bit strange. Maybe a hybrid or incorrect measurement?

# Why multivariate analysis? Problems, Clustering, classification, improved prediction and understanding --------

# Problems: normal models cannot deal well with data with many variables. Problems that arise are colinearity resulting in strange estimates of regression coefficients, with more variables than samples, regression coefficients cannot be estimated at all with multiple linear regression, and with less variables overfitting is likely to occur.
# You can perform clustering: Unsupervised. Example: We don't know whether there are different iris species, but there may be. See if there are different clusters.
# You can perform classification: Supervised. Example: There are three iris species and we want to know how well we can distinguish them based on four characteristics
# improved prediction: including all/many potentially important variables, multivariate advantage & dealing with co-linearity.
# understanding. Example: there are three iris species and we want to know which factors determine where they occur
# if you want to improve prediction but don't need to understand the influence of explanatory factors on the prediction, there are also options. Among others knn (is ok with few data) and neural networks (Data monster).


# problems (colinearity, nsamples<=nvariables, overfitting) --------------------------------
# colinearity -------------------------------------------------------------

xa=rnorm(1000)
xb=xa+rnorm(1000)*0.1
ya=0.3*xa + rnorm(1000)

plot(xa,xb,main='data is colinear')

LM=lm(ya~xa+xb)
library(car)
vif(LM)# should be <=4 for lm!
# issues: poor estimate of regression coefficients. 

summary(LM)# LM says neither variable is significant, one of either may be negatively related to ya

# but in reality, both are very significantly positively related to ya
LMa=lm(ya~xa)
LMb=lm(ya~xb)
summary(LMa)
summary(LMb)

# nsamples<=nvariables & PLS -------------------------------------------------------------

# one option would be to do PCA on the explanatory factors and choose the first PC's, do Y~PC's and then based on that result calculate back Y~X. This is approximately what PCR (principal component regression) does. PLS is a better and smarter version. Already during the process of creating 'PC's' (called LV's in PLS) it takes the Y data into account.
library(pls)
data(gasoline)
dim(gasoline$NIR)# 60 samples<401 explanatory factors
gas1 <- plsr(octane ~ NIR, ncomp = 10, data = gasoline, validation = "LOO") # estimating the regression coefficients with a multivariate model, something like pca and then regression but an improved version of that.
bestmodel=which.min(as.data.frame.table(RMSEP(gas1)[[1]])[(1:7)*2,]$Freq)-1 # here we see at which number of lv's (something like PC's from pca) the pls model predicts best. 
plot(gas1,bestmodel,plottype = "coef",main='PLS best model') # all regression coefficients are successfully estimated


gas1lm <- lm(octane ~ NIR,data = gasoline)
summary(gas1lm) #goes wrong. only a small part of the regression coefficients was estimated. Note that R2 is 1. This data is overfitted.

# PLS warning! Interpretation is difficult and it is mainly used by linear algebra people and statisticians so understandable explanations are rare. Many options of linear models such as including random effects are difficult to implement in PLS or not yet implemented.

# overfitting -------------------------------------------------------------

# good example: https://upload.wikimedia.org/wikipedia/commons/6/68/Overfitted_Data.png
# compare pls with lm for 100 different random samples from the dataset.
for(i in 1:100){
  data(gasoline)
  gasoline=as.data.frame(scale(gasoline)) # pls function from the package I use should do it itself, but to be sure here I do it manually first. Actually even better is to scale the train data, and then use the sd and mean you got from the train data to scale the test data manually (testdatasc=(testdata-meantraindata)/sdtraindata).
  
  # gasoline$octane=gasoline$octane+rnorm(60) # possibility to test with some more noise added to Y
  # gasoline=as.data.frame(scale(gasoline))
  # gasoline[,-1]=scale(matrix(rnorm(401*60),60,401)+gasoline[,-1]*0.8)# possibility to test with some more noise added to X
  
  # select 30 random variables, otherwise lm does not work.
  gasoline=gasoline[,c(1,sample(2:401,30))]
  
  # split data in test and train data. The train data is used to train the model, the test data is used to test the model. This is great for preventing overfitting.
  # divide into random training and test set
  train=sample(1:60,50)
  test=setdiff(1:60,train)
  gasTrain <- gasoline[train,] 
  gasTest <- gasoline[test,]
  gas1 <- plsr(octane ~ ., ncomp = 10, data = gasTrain, validation = "LOO") # pls is a mutlivariate regression method. It is sort of like PCA, the data is 'filtered' by reducing dimensions. 
  bestmodel=which.min(as.data.frame.table(RMSEP(gas1)[[1]])[(1:7)*2,]$Freq)-1 # here we see at which number of dimensions (lv's similar to PC's in PCA) the pls model predicts best. it was found with LOO (leave one out cross validation) on the training set.
  
  # possibility when adding noise
  # if(bestmodel==0){
  #   bestmodel=1
  # }
  
  PRD=predict(gas1, ncomp = bestmodel, newdata = gasTest) # predicting Ytest with Xtest and the model built and optimized with Xtrain and Ytrain.
  CTPLS=cor.test(PRD,gasTest$octane) # testing how well we predicted Ytest with the optimized pls model
  
  # comparing this with multiple linear regression
  gas1lm <- lm(octane ~ .,data = gasTrain)
  P=predict(gas1lm,gasTest)
  CTLM=cor.test(P,gasTest$octane)
  
  #saving results
  if(i==1){
    CTLMop=CTLM[[4]]
    CTPLSop=CTPLS[[4]]
  }else{
    CTLMop=c(CTLMop,CTLM[[4]])
    CTPLSop=c(CTPLSop,CTPLS[[4]])
  }
}

plot(CTLMop,CTPLSop, xlab="LM prediction accuracy",ylab='PLS prediction accuracy',pch=20)
abline(0,1)

# the normal linear model gives worse prediction results than the PLS model


# showing that PLS is related to LM and MLM. The best PLS model is sort of a mix between them. MLM (univariate linear regression) is a special case of PLS (using all components), regression coefficients of LM (univariate linear regression) are usually almost identical (apart form a constant) to PLS using the first component only.

# univariate
coefuniop=c()
colnames(gasTrain)=gsub(" ","",colnames(gasTrain))
for(i in 1:30){
  CN= colnames(gasTrain)[i+1]
  PST=paste("gas1lmuni <- lm(octane ~", CN,",data = gasTrain)",sep="")
  LMuni=eval(parse(text=PST))
  coefuni=LMuni$coefficients[2]
  coefuniop=c(coefuniop,coefuni)
}


# regression coefficients PLS vs LM:
par(mfrow=c(2,3))
plot(coefuniop,type='l',main='LM', ylab='regression coefficient',xlab='explanatory variable')
abline(h=0,col='gray')

plot(gas1lm$coefficients[-1],type='l',main='MLM', ylab='regression coefficient',xlab='explanatory variable') # correlation MLM, LM = 0.17
abline(h=0,col='gray')
plot(gas1,bestmodel,plottype = "coef",main='PLS best model',xlab='explanatory variable') # cor uni pls best=0.66, cor mlm plsbest=0.28. PLS with the optimal number of components >1 is sort of a mix between LM and MLMbut a complicated mix.
plot(gas1,1,plottype = "coef",main='PLS first component',xlab='explanatory variable') # regression coefficient of first PLS component is approximately equal to univariate regression coefficient (here correlation is 0.997, i don't know when it would be lower, I tested a few datasets and the correlation between univariate and pls first component was always very high). 

gas1 <- plsr(octane ~ ., ncomp = 30, data = gasTrain, validation = "LOO") # pls is a mutlivariate regression method. It is sort of like PCA, the data is 'filtered' by reducing dimensions. 
plot(gas1,30,plottype = "coef",main='PLS all components',xlab='explanatory variable') # Regression coefficient of PLS with all components would be equal to regression coefficients of MLM. cor mlm pls with all components =1

# clustering --------------------------------------------------------------

# hierarchical clustering:
par(mfrow=c(1,1))
irissel=iris[,-5]
dist_mat <- dist(irissel, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average') # other options for method: among others: single and complete. Single has the tendency to chain data together, usually not the best option.

library(dendextend)

avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 2) # cutoff h=2 is not entirely objective. There are methods to improve the choice of number of clusters.
plot(avg_col_dend)# visualization of method is nicely done https://www.youtube.com/watch?v=lU8S_Do3OPQ on time stamp 5:52. I did not listen to the video so I cannot judge whether it is a good one.

# normal clustering, using kmeans as example (there are more methods).  k-means clustering minimizes within-cluster variances (squared Euclidean distances)

library(factoextra)
library(NbClust)

# methods to find out how many clusters are appropriate. I copy pasted these lines of code from the internet.
# Elbow method
irissel[,]=scale(irissel)
fviz_nbclust(irissel, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(irissel, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
fviz_nbclust(irissel, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
# other interesting clustering methods: dbscan, self organizing maps, utilization density distributions for animal home range calculation (not really clustering)

# most methods of automatic cluster n selection say 2, so 2 we go with..
KM=kmeans(irissel,2)

PCA_scores$clustering=as.factor(KM$cluster)
ggp=ggplot()+
  geom_point(data=PCA_scores, aes(x=PC1, y=PC2,color=species,shape=clustering))+
  geom_segment(data=PCA_loadings,aes(x=Xbegin,y=Ybegin,xend =PC1 , yend = PC2),arrow = arrow(length = unit(0.05, "npc")),size=1,color='gray')+
  geom_text_repel(data=PCA_loadings,aes(x=PC1,y=PC2,  label = species),size=5,color='black')+
  xlab(paste('PC1 (explained variance=',proportionofvairance_explained_nice[1],'%)',sep=''))+
  ylab(paste('PC2 (explained variance=',proportionofvairance_explained_nice[2],'%)',sep=''))+
  theme_bw() +  
  theme(legend.position="none") +
  theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank())+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
ggp

# see the similarity between the clusters generated by kmeans and the species.
ggp+ theme(legend.position="right")


# classification using knn as an example ----------------------------------------------------------

# with knn you feed the algorithm labeled data (we know which species are which) and then for new data points (we want to know which species it is) knn looks at which k labeled data points it is closest to.

# Problems with classification: Overfitting.
# deal with it! 
# options: splitting data in training and test sets, bootstrapping, permutation, double cross-validation.
irissc=iris
irissc[,-5]=scale(iris[,-5])

# function that can sample 80% of the data randomly.
sample80=function(x){
  sample(x,round(length(x)*0.8))
}
# test with 100 different training and test sets
for(i in 1:100){
  AG=aggregate(1:length(irissc$Sepal.Length),list(irissc$Species),FUN=sample80)
  
  if(class(AG$x[1])=='list'){
    trains=unlist(AG$x)
  }else{
    trains=as.vector(AG$x)
  }
  
  tests=setdiff(1:dim(irissc)[1],trains)
  train=irissc[trains,-5]
  test=irissc[tests,-5]
  library(class)
  # parameter k can be optimized. Best is to do double cross validation in that case.
  # separate data in test and train, further separate train into train train and train test.
  KNNpred=knn(train, test, irissc$Species[trains], k = 10, l = 0, prob = FALSE, use.all = TRUE)
  library(caret)
  
  conf_mat <- confusionMatrix(data=KNNpred, reference = irissc$Species[tests])
  if(i==1){
    conf_matop=+conf_mat$table
  }else{
    conf_matop=conf_matop+conf_mat$table 
  }
}
conf_matop
# conf_matop -->setosa is always correctly identified, the other species are sometimes confused. Virginica is often classified as versicolor, while versicolor is less often classified as Virginica.

# also useful is to find out where it goes wrong, which exact iris plants are misclassified?

# with knn it is very difficult to find out which variables influenced the prediction, because knn is based on a distance transformation of the data.
# with methods such as pls, cca, rda it is easier to find out how variables influenced the prediction.


# Understanding CCA & RDA& DCA-----------------------------------------------------------

# we work with two datasets: varespec and varechem.
# on 24 sites, 44 species were recorded and on these same 24 sites,14 abiotic factors were measured 
# What abiotic factors relate to the presence of these 44 species?
# options: RDA and CCA. They are so called constrained and related to the unconstrained PCA and DCA respectively. Constrained means that the dimension reduction is chosen is such a way that the reduction focuses on variation in species data explained by abiotic factors.
# Choose CCA: if the gradient is long: e.g. some species prefer intermediate moisture conditions. If you sampled moist and dry sites, the relation between moisture and the species occurrence is non-linear (more likely unimodal). 
# Choose RDA: if the gradient is less long: e.g. if you sampled dry and less dry conditions, the relation between the occurrence of these species and moisture is by approximation linear.
# RDA is the only option when you have negative numbers in your data.
# in case of doubt, CCA is best
# [text stolen from: https://www.davidzeleny.net/anadat-r/doku.php/en:ordination] To decide whether to apply linear or unimodal ordination method [dca/cca] on your data, you can use the rule of thumb introduced by Leps & Smilauer (2003): first, calculate DCA (detrended by segments) on your data, and check the length of the first DCA axis (which is scaled in units of standard deviation, S.D.).

# DCA and CCA use the chi-square statistic, that also means that the data should be interpreted relatively (e.g. the cover of Calluna vulgaris is relatively high in this plot compared to other species e.g. try: varespec[1,-1]=0 & varespec[1,1]=100). 

data(varespec)
data(varechem)

# something about transformation in dca: https://ordination.okstate.edu/transfor.htm. Not as neccessary as in PCA.

DCA=decorana(log(varespec+1))# length of first axis is 2.2830 , so using rda is fine or even better, but in this example we will use CCA, because that is what they do in the vegan package and one does not undermine decisions made by the authors of vegan.

x=data.frame(vegan::scores(DCA,display="sites")) 
y=data.frame(vegan::scores(DCA,display="species"))

xrange=apply(x,2,range)[,1:2]
yrange=apply(y,2,range)[,1:2]

y=y/(max(yrange/xrange))
y$species=row.names(y)
y$Xbegin=rep(0,length(y$DCA1))
y$Ybegin=rep(0,length(y$DCA1))

# it is not necessary to represent the species with arrows. For a cleanear plot, using only text labels = OK. Site scores can also be removed from the plot.
ggp=ggplot()+
  geom_point(data=x, aes(x=DCA1, y=DCA2))+
  geom_segment(data=y,aes(x=Xbegin,y=Ybegin,xend =DCA1 , yend = DCA2),arrow = arrow(length = unit(0.05, "npc")),size=1,color='gray')+
  geom_text_repel(data=y,aes(x=DCA1,y=DCA2,  label = species),size=5,color='black')+
  xlab('DCA1')+
  ylab('DCA2')+
  theme_bw() +  
  theme(legend.position="none") +
  theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank())+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

ggp
ggp+geom_text(data=x, aes(x=DCA1, y=DCA2,label=1:24)) # nicer solution for site labels is of course possible


# find out which of the environmental factors correlate with the species composition
EV=envfit(DCA,varechem,choices=c(1,2,3))

# to add them to plot, fist do some coding
EVdf=data.frame(EV$vectors$arrow)

# merging with y, because ggrepel then repels both species and environmental factors at the same time.
EVdf_merg=data.frame(EVdf,species=row.names(EVdf),Xbegin=rep(0,length(EVdf$DCA1)), Ybegin=rep(0,length(EVdf$DCA1)))
y2=rbind(y[,-4],EVdf_merg)

# giving abiotic factors and species different colors and sizes
color=c(rep('black',length(y[,1])),rep('blue',length(EVdf[,1])))
size=c(rep(1,length(y[,1])),rep(2,length(EVdf[,1])))

y3=cbind(y2,color,size)
y3s=y3[-c(1:44),]

# in this plot, it is common to represent the abiotic factors as arrows, and the species not as arrows.
ggp=ggplot()+
  geom_point(data=x, aes(x=DCA1, y=DCA2))+
  # geom_segment(data=y,aes(x=Xbegin,y=Ybegin,xend =DCA1 , yend = DCA2),arrow = arrow(length = unit(0.05, "npc")),size=1,color='gray')+
  geom_segment(data=y3s,aes(x=Xbegin,y=Ybegin,xend =DCA1 , yend = DCA2),arrow = arrow(length = unit(0.05, "npc")),size=1,color='blue')+
  geom_text_repel(data=y3,aes(x=DCA1,y=DCA2,  label = species),size=y3$size*4,color=y3$color)+
  xlab('DCA1')+
  ylab('DCA2')+
  theme_bw() +  
  theme(legend.position="none") +
  theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank())+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

ggp+geom_text(data=x, aes(x=DCA1, y=DCA2,label=1:24))

# some interpretations:
# in site 9 and site 4, humdepth is deep, and species Hylospl and Descflex are likely present with a higher cover than in sites 22 and 20. Hylospl and Descflex are more present in similar sites. pH is low in sites 9 and 4. 

# note that the species data is still the same!


# CCA is a different method for finding out which of the environmental factors correlate with the species composition

# with CCA, the space is chosen in such a way that there is maximum relation between species and explanatory factors. The location of species in the plots will thus change depending on the explanatory factors chosen.The advantage is that big variation in species presence not related to abiotic factors is repressed, while small variation in species presence strongly related to abiotic factors is inflated. This way your plot and results focus more on the relation between abiotic variables and species presence. Disadvantage is that the plot is not unbiased anymore. From the plots you may get the impression that there is a stronger relation between abiotic factors and species than in reality or a strong relation between species themselves that in reality is not really there. It is therefore advisable to look at both cca and dca (or rda and pca in the case of linear method).

# some stealing on my part
## cca 
## Common but bad way: use all variables you happen to have in your
## environmental data matrix
data(varespec)
data(varechem)
varechem=log(varechem+1);varechem[,]=scale(varechem) # added to example. Not neccessary, but I found that with DCA especially, logscaling can improve result.
vare.cca <- cca(varespec, varechem)
vare.cca
plot(vare.cca) # default plot

# my try of improving it.
x=data.frame(vegan::scores(vare.cca,display="sites"))
y=data.frame(vegan::scores(vare.cca,display="species"))

xrange=apply(x,2,range)[,1:2]
yrange=apply(y,2,range)[,1:2]

y=y/(max(yrange/xrange))
y$species=row.names(y)
y$Xbegin=rep(0,length(y$CCA1))
y$Ybegin=rep(0,length(y$CCA1))


EV=data.frame(vare.cca$CCA$biplot)
EVdf=EV[,1:2]
EVdfrange=apply(EVdf,2,range)[,1:2]
EVdf=EVdf/(max(EVdf/xrange))


EVdf_merg=data.frame(EVdf,species=row.names(EVdf),Xbegin=rep(0,length(EVdf$CCA1)), Ybegin=rep(0,length(EVdf$CCA1)))
# add them to plot
y2=rbind(y,EVdf_merg)
color=c(rep('black',length(y[,1])),rep('blue',length(EVdf[,1])))
size=c(rep(1,length(y[,1])),rep(2,length(EVdf[,1])))
y3=cbind(y2,color,size)
y3s=y3[-c(1:44),]

ggp=ggplot()+
  geom_point(data=x, aes(x=CCA1, y=CCA2))+
  # geom_segment(data=y,aes(x=Xbegin,y=Ybegin,xend =CCA1 , yend = CCA2),arrow = arrow(length = unit(0.05, "npc")),size=1,color='gray')+
  geom_segment(data=y3s,aes(x=Xbegin,y=Ybegin,xend =CCA1 , yend = CCA2),arrow = arrow(length = unit(0.05, "npc")),size=1,color='blue')+
  geom_text_repel(data=y3,aes(x=CCA1,y=CCA2,  label = species),size=y3$size*4,color=y3$color)+
  xlab('CCA1')+
  ylab('CCA2')+
  theme_bw() +  
  theme(legend.position="none") +
  theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank())+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

ggp+geom_text(data=x, aes(x=CCA1, y=CCA2,label=1:24))

# some statistics
anova(vare.cca) # model is not significant

# while it is no problem that the species presences are correlated with each other, it is a problem that abiotic factors are related to each other.
vif.cca(vare.cca) # variance inflation factors are too high, abiotic factors should not relate too much to each other.

# different way of showing correlation between variables.
par(mfrow=c(1,1))
library(corrplot)
COR=cor(varechem)
corrplot(COR,'number') # abiotic factors are very related to each other. An option is to subselect those variables that are not too correlated to each other, or better: to choose a specific factor/factors you want to study.

# eg. N, pH, Al
cca_sel=cca(varespec ~ N+pH+Al, varechem)

vare.cca=cca_sel

x=data.frame(vegan::scores(vare.cca,display="sites"))
y=data.frame(vegan::scores(vare.cca,display="species"))

xrange=apply(x,2,range)[,1:2]
yrange=apply(y,2,range)[,1:2]

y=y/(max(yrange/xrange))
y$species=row.names(y)
y$Xbegin=rep(0,length(y$CCA1))
y$Ybegin=rep(0,length(y$CCA1))


EV=data.frame(vare.cca$CCA$biplot)
EVdf=EV[,1:2]
EVdfrange=apply(EVdf,2,range)[,1:2]
EVdf=EVdf/(max(EVdf/xrange))


EVdf_merg=data.frame(EVdf,species=row.names(EVdf),Xbegin=rep(0,length(EVdf$CCA1)), Ybegin=rep(0,length(EVdf$CCA1)))
# add them to plot
y2=rbind(y,EVdf_merg)
color=c(rep('black',length(y[,1])),rep('blue',length(EVdf[,1])))
size=c(rep(1,length(y[,1])),rep(2,length(EVdf[,1])))
y3=cbind(y2,color,size)
y3s=y3[-c(1:44),]

ggp=ggplot()+
  geom_point(data=x, aes(x=CCA1, y=CCA2))+
  # geom_segment(data=y,aes(x=Xbegin,y=Ybegin,xend =CCA1 , yend = CCA2),arrow = arrow(length = unit(0.05, "npc")),size=1,color='gray')+
  geom_segment(data=y3s,aes(x=Xbegin,y=Ybegin,xend =CCA1 , yend = CCA2),arrow = arrow(length = unit(0.05, "npc")),size=1,color='blue')+
  geom_text_repel(data=y3,aes(x=CCA1,y=CCA2,  label = species),size=y3$size*4,color=y3$color)+
  xlab('CCA1')+
  ylab('CCA2')+
  theme_bw() +  
  theme(legend.position="none") +
  theme(panel.grid.minor = element_blank(), panel.grid.major =   element_blank())+
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

ggp+geom_text(data=x, aes(x=CCA1, y=CCA2,label=1:24))

#statistics
anova(vare.cca) # model is significant

# while it is no problem that the species presences are correlated with each other, it is a problem that abiotic factors are related to each other.
vif.cca(vare.cca) # variance inflation is perfect
# "Automatic stepwise model building for constrained ordination methods"
ordistep(vare.cca)# Only Al is significantly related to the species composition


## Is the effect of Al really that important? 
# remove information in species data explained by N and pH first before testing the effect of Al
CCA=cca(varespec ~ Al + Condition(pH+N), varechem)
anova(CCA,by='margin')# still significant --> importance is possible


# for RDA do the same as with CCA but with rda function instead
# other tips: removing or accounting for rare species is an option. I don't know wheather it is smart to do this so you need to check yourself.
# to see how well species are explained by the model, you can use the 'goodness' function. --> no signficance but gives sort of effect size.
barplot(t(goodness(CCA)),las=2,ylab='goodness of fit')

# to get variance explained per axis for DCA, you can sort of cheat by using:
# (David Zeleny): 
# cca (log1p (vltava.spe)) # or cca(whatever transformation you like or no transformation(data))
# notice that there are no constraints and cca basically performs ca, which is related to dca
# results in:
# Call: cca(X = log1p(vltava.spe))
# 
# Inertia Rank
# Total           7.372     
# Unconstrained   7.372   96
# Inertia is mean squared contingency coefficient 
# 
# Eigenvalues for unconstrained axes:
#   CA1    CA2    CA3    CA4    CA5    CA6    CA7    CA8 
# 0.5533 0.4594 0.4131 0.3083 0.2951 0.2576 0.2147 0.2032 
# (Showed only 8 of all 96 unconstrained eigenvalues)

# As you can see, total inertia is 7.372, and if needed, variation captured by particular axes can be calculated as eigenvalue/total inertia (e.g., for the first axis, 0.553/7.372*100 = 7.50%)1)

# this is however not entirely correct: 
# (David Zeleny) "1) However, see this note from Jari Oksanen on this topic, copied from this discussion: The concept of total inertia does not exist in DCA. Alternative software use the total inertia from other ordination methods such as orthogonal correspondence analysis. Just call cca() for your data to get the total inertia of orthogonal CA. However, that really has no relevance for DCA, although that statistics is commonly used and ritually reported in papers."

