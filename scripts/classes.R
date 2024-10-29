#Classes
rm(list=ls())
library(ggplot2)
data<-read.csv("data/dataFall2022.csv")


str(data) #shows for each column the class
 
data$clusterLength #A numeric variable
data$nLeaves #A integer variable
data$fullName #A character variable
class(data$clusterLength) #Check what variable it is using the class function


as.numeric() #Change something to numeric
as.integer() #Change something to integer
as.Date()   #Change something to date
as.double() #Change something to double (A form of numeric)
data$popFactor<-as.factor(data$pop) #Change something to factor!
                                    #Important if you have numeric variables that are not continuos
levels(data$popFactor)              #Check the unique values in a factor

###########################
#Errors
#What if numeric data is not numeric but a character??
head(data) #Look at head of data

#Calculate the cluster area by multiplying cluster length by cluster width
data$clusterWidth*data$clusterLength
#Gives an error
#Error in data$clusterWidth * data$clusterLength : 
#non-numeric argument to binary operator

class(data$clusterLength) #This is numeric as you would expect
class(data$clusterWidth)  #This is a charcater which it shouldn't be

ggplot(data,aes(x=clusterLength,y=clusterWidth))+geom_point()
#The y axis looks weird the numbers are not spaced out and in order

plot(clusterLength~clusterWidth,data=data)
#So this plot looks correct but it does give a warning!
#Warning message:
#In xy.coords(x, y, xlabel, ylabel, log) : NAs introduced by coercion
#If it does something like this check your data!

unique(data$clusterWidth) #Look at the unique values in the column If you look closely you will find "4..2" around place 60

#More complicated code:
#This gets the unique values of clusterwidth for which as.numeric clusterwidth is NA and normal cluster width is not NA

unique(data$clusterWidth[is.na(as.numeric(data$clusterWidth))&is.na(data$clusterWidth)==F])



data$clusterWidth[data$clusterWidth=="4..2"]<-"4.2" #Change the value in R instead of excel!
class(data$clusterWidth)
data$clusterWidth2<-as.numeric(data$clusterWidth) #Don't save over your orignal data if you are not sure it fixed your issue
data$clusterWidth<-as.numeric(data$clusterWidth) #Don't save over your orignal data if you are not sure it fixed your issue

class(data$clusterWidth)

data$ClusterArea<-data$clusterWidth*data$clusterLength
ggplot(data,aes(x=clusterLength,y=clusterWidth))+geom_point()
#Plots look clean
plot(clusterLength~clusterWidth,data=data)
#And do not give errors anymore

#What if things are not factors: this seems to be mostly a base R problem

plot(ClusterArea~pop,data)
class(data$ClusterArea)
class(data$pop)
data$popFactor<-as.factor(data$pop)

plot(ClusterArea~popFactor,data)
unique(data$popFactor)
levels(data$popFactor)

#This is mostly important of your x variables are numbers but should be treated as non-continuous
#I.e. Quadrat in this data set

plot(ClusterArea~quadrat,data)
data$quadratFactor<-as.factor(data$quadrat)
plot(ClusterArea~quadratFactor,data)


#########################
#some work at changing variables to different things:
#Dates 

unique(data$date) #Check unique values
#Format is the standard excel data output (For english excel your milage will vary)

unique(as.Date(data$date)) #Check if basic conversion works
#It does not

as.Date(data$date,format = "%d/%m/%y") #This uses only the first 2 numbers of year 2022 becomes 2020

data$date2<-sub(pattern = "2022",replacement = "22",data$date) #Use the sub function to replace 2022 with 22

as.Date(data$date2,format = "%d/%m/%y") #This fixes it!
data$date2<-as.Date(data$date2,format = "%d/%m/%y") 
class(data$date2)
data$date2-data$date2[1] #you can now calculate things as if it were a date in excel!

as.Date(data$date2,format = "%D/%M/%Y") #Using capital letters also works


ggplot(data,aes(x=date2,y=nInfl))+geom_point() #Makes the plots look nice
boxplot(nInfl~date,data)