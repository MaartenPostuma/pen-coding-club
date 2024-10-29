rm(list=ls())
#Creating functions
dataSpring<-read.csv("data/dataSpring2022.csv")

#We can use functions in R to calculate things that we need such as
mean(dataSpring$nLeaves,na.rm=T) #Mean
sd(dataSpring$nLeaves,na.rm=T)   #SD

aggregate(nLeaves~pop,dataSpring,mean)
aggregate(nLeaves~pop,dataSpring,sd)

#However some functions are not in R (such as standard error of the mean (SEM))
#Standard error of the mean = Standard deviation / square root of the number of samples

sd(dataSpring$nLeaves,na.rm=T)/sqrt(length(dataSpring$nLeaves)) #A lot to write!

sd(dataSpring$nLeaves[dataSpring$pop=="ardal"])/sqrt(length(dataSpring$nLeaves[dataSpring$pop=="ardal"])) #Especially if we want to do this for every populations
#But we can make a function of this ourselves!!!

function(x){
  
}

x<-dataSpring$nLeaves #We make a variable called x
standardError<-sd(x,na.rm=T)/sqrt(length(x)) #We calculate the standard deviation using x 
standardError #Return the standard error


SE<-function(x){ #The function is the same but instead in a function
  standardError<-sd(x,na.rm=T)/sqrt(length(x)) #for the x input in the function
  return(standardError) #The code is run and standard error is returned as output
}




SE(x=dataSpring$nLeaves) #By saying x=dataSpring$nLeaves we basicly do the same thing as above
aggregate(nLeaves~pop,dataSpring,SE) #We can use this for aggregate


#But is the function correct???
SE(dataSpring$clusterLength) 
length(dataSpring$clusterLength)  #This does not exclude NAs from the length

#Make a function to count NAs 
x<-dataSpring$clusterLength  #X is the cluster length
sum(is.na(x)==F) #Sum all Trues for is.na ==F 
sum(is.na(x))
length(x)

countNonNAs<-function(x){ #Change it into a function
  return(sum(!is.na(x)))
}
#Now we can count the NAs!
countNonNAs(dataSpring$nLeaves)
countNonNAs(dataSpring$clusterLength)

#We can use the count NA function in another function
SENew<-function(x){
  standardError<-sd(x,na.rm=T)/sqrt(countNonNAs(x))
  return(c(standardError))
}
#Big differences. It is super important to check if your function do what you expect them to do
SE(dataSpring$nLeaves)
SENew(dataSpring$nLeaves)
SE(dataSpring$clusterLength)
SENew(dataSpring$clusterLength)


aggregate(nLeaves~pop,dataSpring,SENew)

#Other functions that I tend to use:

countUniques<-function(x){length(unique(x))}

countUniques(x=c("blah","blahblah","blah"))
length(unique(c("blah","blahblah","blah")))
#Having mulitple inputs/conditions ################################

#Use dataFall to do something different in season fall compared to season summer 
dataFall<-read.csv("data/dataFall2022.csv")
season<-"fall"

SE_Extra<-function(x,season){ #Depending on wether season is fall or spring it 
  if(season=="fall"){         #Returns something different
    return("i'm in fall")
  }else{
    standarError<-sd(x,na.rm=T/sqrt(countNonNAs(x)))
  }
  return("I'm somewhere in spring")
  }


SE_Extra(x=dataFall$nLeaves,season="fall")
SE_Extra(x=dataFall$nLeaves,season="spring")


#Now we can use this to exclude all new individuals in fall
#We also want a specify which column to do this for.
SE_Extra<-function(data,column,season){
  if(season=="fall"){
    standardError<-sd(data[data$prevNum!="new",column],na.rm=T)/sqrt(countNonNAs(data[data$prevNum!="new",column]))
  }else(
    standardError<-sd(data[,column],na.rm=T)/sqrt(countNonNAs(data[,column]))
  )
  return(standardError)
}
#These give different results. Try to think of why this occurs

SE_Extra(data=dataFall,column="nLeaves",season="fall")
SE_Extra(data=dataFall,column="nLeaves",season="spring")
SE_Extra(data=dataSpring,column="clusterLength",season="spring")



#Another exmaple with for loops
#Determine vegetation type based on two different columns


column1<-c("trees","grass")
column2<-c("trees","trees")
vegetationType<-function(column1,column2){
  if(column1=="grass"){return("grassland")}
  if(column1!="grass"&column2=="trees"){return("forest")}
  
  
}

#For the first values in column1 and 2 (both trees we get forest)
#For the 2nd values in column 1 and 2 (Grass and trees we get grassland)
vegetationType(column1[1],column2[1])
vegetationType(column1[2],column2[2])


#We can for loop this
column3<-c() #Create a column to save this in
for(i in 1:length(column1)){
column3[i]<-print(vegationType(column1[i],column2[i]))
}  
  
column3
