rm(list=ls()) # Removes everything from your memory

#data.frames!


#loading a data.frame
#If file is saved a comma seperated values
data<-read.csv("where/to/find/your/file.csv") #If file is saved as comma separated values in excel
data<-read.csv2("where/to/find/your/file.csv") #If file is in a slightly different format 


soilData<-read.csv("data/soilDataAll.csv") #This data is in the github aswell! #Is what I will to show +

climateData<-read.csv("data/climateData.csv")  
#NOTES: location needs to be written with / if you copy from the directory name the / will be \ which won't work


#If file is a normal excel file  

library(readxl)

dataset <- read_excel("where/to/find/your/file.xlsx")
#Or using the import data set button


#Quick look at the data
View(soilData) #opens new tab with the data.frame
soilData
head(soilData,n = 3) #Show first 6 lines in the consoloe below
tail(soilData) #Show last 6 lines

nrow(soilData) #Number of rows in the data

ncol(soilData) #Number of columns in the data
dim(soilData) #Get norw and number of columns
colnames(soilData) #Get the colnames
summary(soilData) #Gives some statistics on the whole data.frame


#Slightly more important
str(soilData)  #Gives an overview of what each column is (What class etc.)



#Look at a single column
soilData$pop2 #Using $ 
soilData$organic.matter_.

soilData[,1]  #Getting the first column
soilData[,"pop2"] #Writing the column name


soilData[,c("pop2","method","depth","pH")] #Get multiple columns
vectorTest<-c("pop2","method","depth","pH")
vectorTest
soilData[,vectorTest] #Get multiple columns using the vectorTest

#Concatenate

#Subset data rows
soilData[,1]  #Getting the first column
soilData[1,]  #Getting the first row



soilData$depth=="0-10" #Returns true and falses for each value in soilData$depth 
sum(soilData$depth=="0-10")


which(soilData$depth=="0-10") #Returns the "item numbers" of all the TRUE in the previous code

depth10<-soilData[soilData$depth=="0-10",] #Square brackets mean for which

areTheseNAs<-is.na(depth10$organic.matter_.)
areTheseNAs

depth10[!areTheseNAs,]

depth10[is.na(depth10$organic.matter_.)==F,] #Remove rows with NAs

waterData0_10<-soilData[soilData$depth=="0-10"&soilData$method=="water",] #Subset the data set for 2 questions

waterData20_30<-soilData[soilData$depth=="20-30"&soilData$method=="water",] #Also do it for 20-30 depth

subset(soilData,soilData$depth=="0-10") #Different way to subset. It does the exact same thing
 

#Subset data columns

onlyNumbers<-soilData[,nchar(colnames(soilData))<4] #Only columns for which the colname is <3
head(onlyNumbers)

#Combine dataframes

head(climateData)
length(climateData$pop2) #What is the length of an object
length(waterData20_30$pop2)

vector1<-c(1,2,3)
vector2<-c(4,3,2)

vector1==vector2
vector1%in%vector2


climateDataInSoil<-climateData[climateData$pop2%in%waterData20_30$pop2,]#Subset climateData to only include the values in climateData$pop2 that also occur in waterData$pop2

waterData20_30<-waterData20_30[order(waterData20_30$pop2),] #Order data to make sure it works when cbinding
climateDataInSoil<-climateDataInSoil[order(climateDataInSoil$pop2),]
waterAndClimate<-cbind(climateDataInSoil,waterData20_30) #Bind columns to the end of the other data.frame


allWaterData<-rbind(waterData0_10[,c(2,1)],waterData20_30[,c(1,2)]) #Bind rows under the data.frame

#Data needs be the same size, the same number of rows for cbind same nubmer of columns for rbind



waterAndClimate<-merge(climateDataInSoil,waterData20_30,by="pop2") #Combine data.frames based on information in one column #This is intelligent cbind!

#Some more statistics 

rowMeans(onlyNumbers,na.rm=T) 
rowSums(onlyNumbers,na.rm=T)
colMeans(onlyNumbers,na.rm=T)
colSums(onlyNumbers,na.rm=T)


aggregate(pH~depth*method,data=soilData,FUN = mean)
#The coolest function!
