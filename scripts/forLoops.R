rm(list=ls())
data<-read.csv("data/PossibleChosenMothers.csv")
data


#What is a for loop
#Does the same thing multiple times for instance if we want to do something for
i<-1 #i is 1, i is 3, i is 8 and i is 22 you could write it like this. However if we now
print(i) #Want to change something we need to chance it multiple times
i<-3 #it is a lot of writing
print(i) 
i<-8
print(i)
i<-22
print(i)

for (i in c("blah","blahblah","blahblahblah",22)){ #This is a for loop which does the same thing. For i in 1,3,8 and 22 it will run the code between 
print(i) #the curly brackets at the end. So the first time it goes trough it will print i = 1 second time i =3 etc.
}

for(something in c("value","value2")){
  print(something)
}


pop<-"Sma"
pop<-"Hel"
x<-0
for(pop in unique(data$pop)){ #You can use this for many different goals but I mostly use it for subsetting data
  print(pop)
dataSub<-data[data$pop==pop,] #So this code uses all unique values of pop to subset a dataframe afterwhich it plots 
plot(totalWeight~seedWeight,dataSub)  #the data for each subset 
}

#To write a for loop it is easiest to first write the code for a single instance i.e. quadrat is 10 in this case
#If the code works it is "likely" to work for all other instances aswell

dataOut<-data.frame() #We create an empty data.frame in which to save the output data
quadrat<-10
dataSubset<-data[data$quadrat==quadrat,] #We subset the data based on quadrat
dataTemp<-data.frame(quadrat=quadrat,    #And than make a new data set with the mean for each population 
                     meanSeedWeigth=mean(dataSubset$seedWeight)) #To save it in a new data.frame
dataOut<-rbind(dataOut,dataTemp) #We then rbind it to an empty data.frame called dataOut to save it 

#We can then use this code in a forloop to make it work for all different values in quadrat
dataOut<-data.frame()
for(quadrat in unique(data$quadrat)){
  dataSubset<-data[data$quadrat==quadrat,]
  dataTemp<-data.frame(quadrat=quadrat,
                       meanSeedWeigth=mean(dataSubset$seedWeight))
  dataOut<-rbind(dataOut,dataTemp)
  print(dataOut)
}  



#This is a more complicated example in which I used for loops to generate labels based on the number of germinated seeds from each different mother
labelsOut<-data.frame()

for(i in 1:nrow(data)){
  subData<-data[i,]
labels<-data.frame(name=paste0(subData$pop," Q_",subData$quadrat," M_",subData$number),indNumber=paste0("ind_",1:subData$germination.12.6))
labelsOut<-rbind(labelsOut,labels)
}


## While loops ###############

#While loops function slightly different to for loops as they will keep running indefenitly until a condition is met
i<-0
while(i<100){ #While i < 100 keep running the loop
i<-i+1 #Add 1 to i each time it runs
print(i) #print i
} #As you can see the code stops running when i == 100

#You can also use if statements to do this
i<-1
test<-TRUE #Start with test is true
while(test==TRUE){  #run the loop while test== true
  i<-i+1  #Add 1 to i. 
  if(i>100){test<-FALSE} #When i > 100 test becomes false
  print(paste(i,test)) #Print i and test to see when it changes
}

