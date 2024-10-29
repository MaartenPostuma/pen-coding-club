rm(list=ls())
library(ggplot2)
library(ggfortify)
theme_set(theme_bw())
#Multivariate analysis / PCA

#What to do when you have a lot variables/plots that you want to visualize?
#Multi-dimensional scaling / principle components analysis!
data(iris) #Test data set 
pairs(iris,col=iris$Species) #Some variables complicated to see patterns this way. Imagine if you have 20 soil variables!!


pca<-prcomp(iris[,-5],scale=T)
autoplot(pca, data = iris, colour = 'Species', loadings = TRUE,loadings.label = TRUE)+
  coord_fixed()#You have probably seen many of these types of plots already, but what do they mean???

#Shoutout to Wiene (check the multivariate analysis script in the github for a much more thourough explanation! and more complex things aswell!)


xa=rnorm(1000) #create 1000 random numbers with standard deviation 1 and mean 0
hist(xa) # histogram of xa, should be normally distributed
ya=0.8*xa + rnorm(1000,mean=10) # create ya which is somewhat related to xa.
xb=xa
yb=0.8*xb + rnorm(1000,mean=-10)+2.5
group=c(rep(0,1000),rep(1,1000))
DAT=data.frame(X=c(xa,xb),Y=c(ya,yb),Group=group)
plot(xa,ya,asp = 1,pch=20)


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

mod0 <- my_pca <- prcomp(data.frame(xa,ya), scale = T,center = T, retx = T)
autoplot(mod0,loadings=T,loadings.label=T)


#Let's go back to the iris example:

#How to make a PCA:
#PCA's only take numerical variables so we need to "lose" the species name.

pairs(iris,col=iris$Species) #Some variables complicated to see patterns this way. Imagine if you have 20 soil variables!!

prcomp(iris)
irisNoSpec<-iris[,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")]
irisNoSpec<-iris[,c(1,2,3,4)]
irisNoSpec<-iris[,-5]
irisNoSpec<-iris[,colnames(iris)!="Species"]
irisNoSpec<-iris[,sapply(iris,is.numeric)]

row.names(irisNoSpec)<-paste(iris$Species,1:nrow(iris))
#Many more options

pca<-prcomp(irisNoSpec,scale=T) #This is the basic R function
biplot(pca)
library(vegan)
pca2<-rda(irisNoSpec,scale=T)


biplot(pca) #Look terrible...
biplot(pca2,type=c('text','points'),scaling ='sites') #Look a bit better but still not ideal the type and scaling have to do with the rotation, of the axis. This is quite complicated but it has something to do with the interprationability of the PCA. The default R function is not completely ideal... 


library(ggplot2) #Because we like everything ggplot
library(ggfortify) #Because ggplot does not like PCA's

autoplot(pca,data=iris,col="Species",loadings=T,loadings.label=T)+
  coord_fixed()
# install.packages("remotes")
# remotes::install_github("gavinsimpson/ggvegan")
library(ggvegan) #Because ggplot does not like vegans
autoplot(pca2,data=iris,arrow=T,colour = factor(Species)) #We can make this plot prettier but it takes a lot of work!!
#I can show the work flow if you want but for now we continue with the other                                          #Plots look the same but the arrow length differs. (Arrow length does not mean anything it is the relation between the arrows)

data<-iris
df<-iris[c(1, 2, 3, 4)]
PC<-prcomp(df)
PC$x  
PCi<-data.frame(PC$x,Species=data$Species)





plot(pca)
barplot(pca$sdev/sum(pca$sdev)~c(1:4),xlab=("PCA Axis"),ylab="Percentage variance explained")


#How do we interpret this plot?
autoplot(pca,data=iris,col="Species",loadings=T,loadings.label=T)+
  coord_fixed()

plot(Sepal.Width~Species,iris)

plot(Petal.Length~Petal.Width,iris)

pca<-prcomp(irisNoSpec,scale=T) #This is the basic R function
