rm(list=ls()) #Clear the environment
dataFall<-read.csv("data/dataFall2022.csv") #Load data
dataSpring<-read.csv("data/dataSpring2022.csv") #Load data
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9","#009E73",
                       "#0072B2", "#CC79A7","#D55E00","#F0E442") #My personal color scheme

dataSpring #Check data
summary(dataSpring) #Summary data
str(dataSpring) #Check if all of the columns are the right class
str(dataFall) #There is an issue with clusterWidth in the dataFall data set. 

unique(dataFall$clusterWidth) #There is one value that's 4..2 instead of 4.2

dataFall$clusterWidth[dataFall$clusterWidth=="4..2"]<-4.2 #Change it (dataFall clusterwidth for which data fall cluster width is 4..2 becomes 4.2)
dataFall$clusterWidth<-as.numeric(dataFall$clusterWidth) #Make the column numeric
str(dataFall) #Check if it is okay
class(dataFall$clusterWidth)

#Plots #################################

#Create boxplots
boxplot(dataFall$nRosettes~dataFall$pop) #number of rosettes as a function op populations

boxplot(nRosettes~pop,data=dataFall)  #Move data behind the formula, less typing!
boxplot(nRosettes~pop,data=dataSpring) #Change to the data spring data

boxplot(nRosettes~pop,data=dataFall,col=colorBlindBlack8) #Add a colour scheme to the plot

#Scatter plot
plot(clusterLength~clusterWidth,data=dataFall) #Make a scatter plot (2 different numerical variables)

#Create a barplot #######################
#Calculate the mean of the populations
boxplot(nRosettes~pop,data=dataFall)  #Move data behind the formula, less typing!
meanRosettes<-aggregate(nRosettes~pop,data=dataFall,mean) #Way of writing is the same as with the plots!!!
barplot(nRosettes~pop,data=meanRosettes,col=colorBlindBlack8)

#Add error bars 

#I WOULD NOT RECOMMED THIS IN BASIC R BUT THIS IS HOW YOU DO IT

sdRosettes<-aggregate(nRosettes~pop,dataFall,sd) #Calculate standard deviation using aggregate
meanRosettes$sd<-sdRosettes$nRosettes #Store the calculated sds as a new column in meanRosettes
base_r_barplot<-barplot(nRosettes~pop,meanRosettes, #Same formula as above
                        ylim=c(0,max(meanRosettes$nRosettes+meanRosettes$sd)), #Add space for the error bars
                         col=colorBlindBlack8) #Make colours
#This makes the error bars
arrows(x0 = base_r_barplot,  #This specifies where it starts (Takes this from the plot above)
          y0 = meanRosettes$nRosettes + meanRosettes$sd, #top of the error bar
        y1 = meanRosettes$nRosettes, #Bottom of the error bar
        angle = 90,code = 1,length = 0.1) #Some graphical parameters



#Check all variables 
colnames(dataFall) #What are the variable names again?
boxplot(nLeaves~pop,data=dataFall,col=colorBlindBlack8) #Change the y variable keep pop the same
boxplot(lengthLongestLeaf~pop,data=dataFall,col=colorBlindBlack8)
boxplot(nInfl~pop,data=dataFall,col=colorBlindBlack8)
boxplot(lengthInfl~pop,data=dataFall,col=colorBlindBlack8)


#Some quick and dirty statistics #######################################
#Note how the formula is the same as in the plots and aggregate functions!!!

aov1<-aov(nLeaves~pop,data=dataFall) #Anova (is good when you have catagorical variables as explanatory such as site or population) 
summary(aov1) #Get the anova output (is it significant???)


plot(clusterLength~clusterWidth,data=dataFall) #For scatterplots (numerical x numerical data I recommend lms)
lm1<-lm(clusterLength~clusterWidth,data=dataFall) #
summary(lm1)



##Bonus ########################

#add colours to a scatter plot

dataFall$colour<-"black" #Make a new column that is black
dataFall$colour[dataFall$pop=="ardal"]<-"red" #For each population add a colour
dataFall$colour[dataFall$pop=="laerdal"]<-"blue" #etc...
plot(clusterLength~clusterWidth,data=dataFall,col=colour)


#One liner that I thought of later in the day! To make a column

dataFall$colourOneLiner<-factor(dataFall$pop,labels = colorBlindBlack8) #Changes the character in to a factor
#Then labels all of the different levels using the colourscheme I had.
plot(clusterLength~clusterWidth,data=dataFall,col=colourOneLiner) #Make the pretty plot with the different colours
