rm(list=ls())
#There is no code for this workshop
#Except this code could be used for Q2

#Q2
#Read in the data
obs1=c(0,2,0)
obs2=c(3,0,0)
obs3=c(0,1,3)
obs4=c(0,1,2)
obs5=c(-1,0,1)

#Get distances from point (0,0,0)
#We are using the distance formula shown in Workshop 4 Meeting
#distance=sqrt((x1-y1)^2+(x2-y2)^2+(x3-y3)^2)
#Same as
#distance=sqrt(sum((x-y)^2))
testpoint=c(0,0,0)
d1=sqrt(sum((obs1-testpoint)^2))
d2=sqrt(sum((obs2-testpoint)^2))
d3=sqrt(sum((obs3-testpoint)^2))
d4=sqrt(sum((obs4-testpoint)^2))
d5=sqrt(sum((obs5-testpoint)^2))
d1;d2;d3;d4;d5

#a
#Nearest neighbour is obs5
#Since d5 has the smallest distance from the test point
#obs5 is Low so we record the testpoint as Low 

#b
#3 nearest neighbours are obs 1,4 and 5
#d1,d4,and d5 are the 3 lowest
#They are High, High and low
#So record the testpoint as High