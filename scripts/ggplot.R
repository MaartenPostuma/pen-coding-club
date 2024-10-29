rm(list=ls())
install.packages("ggplot2") #If not installed yet use this function

library(ggplot2)
require(ggplot2)

data<-read.csv("data/dataCombined.csv")

str(data) #Data set containing some information of populations, climate data and soil data.

##################################################

ggplot() #Creates an empty "canvas" 


ggplot(data,aes()) #specify what the data is and add "aesthetics"

ggplot(data,aes(x=latitude ,y=longitude)) #Specifies things on the canvas. In this case latitude and longitude

ggplot(data,aes(x=latitude ,y=longitude)) +
  geom_point() #geom takes information from the ggplot and uses it to plot.


ggplot(data,aes(x=latitude ,y=longitude,col=meta.population))+geom_point() #Add colour to the ggplot aes() so geom_point knows to use different colours. #Meta.population is a factor so you get different colours




ggplot(data,aes(x=latitude ,y=longitude,col=Al))+geom_point() #Aluminium concentration is continuos how colour becomes a gradient.


ggplot(data,aes(x=latitude ,y=longitude,col=Al,size=Al))+geom_point() #Aluminium concentration is continuos how colour becomes a gradient.

ggplot(data,aes(x=latitude ,y=longitude,col=Al,size=Al,shape=meta.population))+geom_point() #Aluminium concentration is continuos how colour becomes a gradient.

#Non-continuous variabels on the x axis
#What geoms to use
ggplot(data,aes(x=meta.population ,y=Al,col=meta.population))+geom_point() #Makes boxplot

ggplot(data,aes(x=meta.population ,y=Al,col=meta.population))+geom_boxplot() #Makes boxplot




ggplot(data,aes(x=meta.population ,y=Al,fill=meta.population))+geom_boxplot() #Makes boxplot

ggplot(data,aes(x=meta.population ,y=Al,fill=meta.population))+geom_violin() #Makes violin plot


ggplot(data,aes(x=meta.population ,y=Al,fill=pop2))+geom_col() #Makes a bar plot (But this sums all of the different values)

ggplot(data,aes(x=pop2 ,y=Al,fill=pop2))+geom_col(position="dodge") #Split the different populations using another aesthetic 





#Many more geoms available check a ggplot cheatsheet to find information



#Using facet_Grid/facet_wrap to create seperate facets for each meta.population
ggplot(data,aes(x=pop2 ,y=Al,fill=meta.population))+
  geom_bar(stat="identity")+
  facet_grid(~meta.population) 


ggplot(data,aes(x=pop2 ,y=Al,fill=meta.population))+
  geom_bar(stat="identity") +
  facet_grid(~meta.population,scales = "free")



ggplot(data,aes(x=pop2 ,y=Al,fill=meta.population))+
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(.~meta.population)



ggplot(data,aes(x=pop2 ,y=Al,fill=meta.population))+facet_wrap(.~meta.population,scales = "free")+
  geom_bar(stat="identity",position="dodge") 


ggplot(data,aes(x=pop2 ,y=Al,fill=meta.population))+facet_wrap(.~meta.population,scales = "free_x")+
  geom_bar(stat="identity",position="dodge") 





#####################################################################
#make plots prettier
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9","#009E73",
                       "#0072B2", "#CC79A7","#D55E00","#F0E442") #My personal color scheme



ggplot(data,aes(x=latitude ,y=longitude,col=meta.population))+geom_point() 

ggplot(data,aes(x=latitude ,y=longitude,col=meta.population))+geom_point(size=4) 

ggplot(data,aes(x=latitude ,y=longitude,col=meta.population))+geom_point(size=4)+
  scale_colour_manual("",values=colorBlindBlack8)


ggplot(data,aes(x=latitude ,y=longitude,col=meta.population))+geom_point(size=4)+
  scale_colour_manual("",values=colorBlindBlack8)

library(ggpubr)


ggplot(data,aes(x=latitude ,y=longitude,col=meta.population))+
  geom_point(size=4)+
  scale_colour_manual("",values=colorBlindBlack8)+
  theme_pubclean()+
  xlab("Latitude%$@")+
  ylab("Longitude  )()$")+
  ggtitle("Longitude + Latitude plot")


##################################################################
#Other packages that work very well in conjunction with ggplot
library(reshape2)

melt() #converts wide data to long data (maybe my favourite function)

str(data)
#Get the columns with the pop names metapopulation and the differnet soil 
dataSoilConc<-data[c("pop2","meta.population","Al","Ca","Fe","K","Mg","Mn","P","S","Si","Zn","Na")]

moltenData<-melt(dataSoilConc,id.vars = c("pop2","meta.population"))

ggplot(moltenData,aes(x=meta.population,y=value,fill=meta.population))+geom_boxplot()+facet_wrap(.~variable,scales="free")
  
data(iris)

ggplot(iris,aes(x=Species,y=Petal.Length))+
  geom_point()

irisMeans<-aggregate(Petal.Length~Species,iris,mean)
irisMeans$sd<-aggregate(Petal.Length~Species,iris,SE)$Petal.Length
library("plotrix")

data<-c(NA,NA,1,2,3,4,5,3,21,3,21,3,5,12,3,15,7,15)
sum(is.na(data)==F)
SE(data)
std.error(data)


SE<-function(x){
  return(sd(x,na.rm=T)/sqrt(sum(!is.na(x))))
}

save<-aggregate(Petal.Length~Species,iris,sd)
save$Petal.Length

ggplot(irisMeans,aes(x=Species,y=Petal.Length,fill=Species))+
  geom_col()+
  geom_errorbar(aes(ymin=Petal.Length-sd,ymax=Petal.Length+sd),width=0.5)


#cheat package
#install.packages("htmltools")
library(esquisse)

esquisser()


