rm(list=ls())

#RBasics
install.packages("")
library() #Loads a library
in #Use in combination with for loops
function(x){} #Create a function
for (i in 1){}# Create a forlopo
while() #While something is true make a for loop

#Different colours  (#Makes things a comment)

"Do different things" #Is a string
andHaveDifferentFunctions #Is an object

thisIsANumber<-1 #Make something a number
thisIsANumber #Check the output in the console

x<-y #x becomes y
x=y #x becomes y

testDataFrame<-data.frame(x=1:10,y=10:10) #Make a quick data.frame
$ and [] # Call things from a data.frame 
testDataFrame$x #Call the x column
testDataFrame$y #Call the y column
testDataFrame[,"y"] #Call the y column
testDataFrame[,1] #Call the first column
testDataFrame[1,] #Call the first row


testList<-list(testDataFrame,testDataFrame) #Make a list of the test data.frame twice

[[]] #Call elements from a list
testList[[1]] # Call the first element of the list

mean(testDataFrame$x) # Use a funcit


() #Use object as function
{} #start end new function, loops, ifelse statements
@ #Access something weird
  
~  #As a function of (define y/x in basic r plots and linear models)
; #Line break on a line

plot(y~x,data=testDataFrame) ; lm(y~x,data=testDataFrame)

""  ''#Write a string
x:y #integers from x to y
-1:-9 #integers from -1 to -9



#Operators
+ #Plus
- #minus
^2 #Squared
* #Times
/ #divided by


== #Equal to 
!= #Not equal to
> #greater than
< #smaller than
>= #Equal greater than
<= #Equal smaller than
x%in%y #Does x value occur in y
x & y #And 
x | y #Or 
is.na() #is a value NA?


$`x pretty`
