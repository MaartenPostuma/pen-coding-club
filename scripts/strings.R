#Strings
rm(list=ls())
library(ggplot2)
dataPopulations<-read.csv("data/stringTest.csv")
dataPopulations
#a lot of info but not in a nice format
#We want a column specifying the season

unique(dataPopulations$season) #Check values

dataPopulations$seasonShort<-NA #Make a new column with NA
dataPopulations$seasonShort[grep("spring",dataPopulations$season)]<-"spring" #Grep the rows that contain spring make them spring
dataPopulations$seasonShort[grep("fall",dataPopulations$season)]<-"fall" #Grep the rows that contain fall make them fall

grepl("spring",dataPopulations$season) #Grepl gives a True False whether a string includes the pattern

dataPopulations$seasonShort2<-sub("2022","",dataPopulations$season) #Change all 2022 for "" (remove 2022)
dataPopulations$seasonShort2<-sub("[0-9]","",dataPopulations$season) #Change all 2022 for "" (remove 2022)
dataPopulations$seasonShort2<-gsub("[0-9]","",dataPopulations$season) #Change all 2022 for "" (remove 2022)
dataPopulations$year<-gsub("[A-z]","",dataPopulations$season) #Change all 2022 for "" (remove 2022)


#column specifying plot number
dataPopulations$plot<-substr(x=dataPopulations$plot.pop.indNum,start = 1,stop = 2) #get first two characters (number can include 10)
dataPopulations$plot
dataPopulations$plot<-sub("-","",dataPopulations$plot) #Remove the - that is left from the code above

sub("-.*$","",dataPopulations$plot.pop.indNum) #Remove everything after -
sub("^.*-","",dataPopulations$plot.pop.indNum) #Remove everything before -

stringSplitList<-strsplit(dataPopulations$plot.pop.indNum,split="-") #Split string in 3 columns  (is saved as a list)

stringSplitDF<-data.frame(do.call(rbind,stringSplitList)) #Transform the list into a matrix (do.call) and the matrix into a data.frame
colnames(stringSplitDF)<-c("plot2","pop2","indNum") #Change the column names
dataPopulations<-cbind(dataPopulations,stringSplitDF) #Add the columns to the data.frame

#column with country
unique(dataPopulations$pop) # each pop contains a country (2 letters) and a number, capitalisation is different

countryTemp<-substr(dataPopulations$pop,1,2) #Get the country code
dataPopulations$Country<-toupper(countryTemp) #Make capitalisation the same for all countries
tolower(countryTemp) #make everything lower cap
unique(dataPopulations$Country) #


#column with full population name
dataPopulations$fullPopName<-gsub("[0-9]","",dataPopulations$fullNameQuadrat) #Remove all numbers from string
sub("[A-z]","",dataPopulations$fullNameQuadrat) #Remove first letter (sub only does the first occurence of a character)
gsub("[A-z]","",dataPopulations$fullNameQuadrat) #Remove all lters (gsub removes all ocurrrences)


dataPopulations$fullPopName
dataPopulations$indNum
#Column with full population name + individual number
paste(dataPopulations$fullPopName,dataPopulations$indNum,sep="-") #Combine two columns seperation is a "-" in this case
paste0(dataPopulations$fullPopName,dataPopulations$indNum) #Combine two columns with no seperation

#########################
#some work at changing variables to different things:
#Dates 

unique(dataPopulations$date) #Check unique values
#Format is the standard excel dataPopulations output (For english excel your mileage will vary)
ggplot(dataPopulations,aes(x=date,y=nLeaves))+geom_point()

unique(as.Date(dataPopulations$date)) #Check if basic conversion works
#It does not
?as.Date
as.Date(dataPopulations$date,format = "%d/%m/%y") #This uses only the first 2 numbers of year 2022 becomes 2020

dataPopulations$date2<-sub(pattern = "2022",replacement = "22",dataPopulations$date) #Use the sub function to replace 2022 with 22

as.Date(dataPopulations$date2,format = "%d/%m/%y") #This fixes it!
dataPopulations$date2<-as.Date(dataPopulations$date2,format = "%d/%m/%y") 
class(dataPopulations$date2)
dataPopulations$date2-dataPopulations$date2[1] #you can now calculate things as if it were a date in excel!

dataPopulations$date3<-as.Date(dataPopulations$date2,format = "%D/%M/%Y") #Using capital letters also works

class(dataPopulations$date2)
ggplot(dataPopulations,aes(x=date2,y=nLeaves))+geom_point() #Makes the plots look nice
boxplot(nLeaves~date2,dataPopulations)

colnames(dataPopulations)<-gsub("","",colnames(dataPopulations))
colnames(dataPopulations)[1]<-c("SEASON")
#There are probably more functions but I don't really use more than these.

# #######################
# # Code to change the data set to the "example dataset"
# data1<-read.csv("data/dataFall2022.csv")
# data1$pop<-factor(data1$pop,labels=c("no1","NO2","No3","No4","Sw1","sw2","Sw3","SW4")) #This uses the factor to quickly relabel manythings
# data1$`plot-pop-indNum`<-paste(data1$quadrat,data1$pop,data1$num,sep="-")
# 
# data1$season[sample(1:nrow(data1),nrow(data1)/2,replace=F)]<-"spring2022"
# dataOut<-data1[,c("season","pop","plot-pop-indNum","fullName","nLeaves","date"),]
# colnames(dataOut)[4]<-"fullNameQuadrat"
# dataStringTest<-dataOut[seq(1,1400,by=7),]
# write.csv(dataStringTest,"data/stringTest.csv",row.names=F)
