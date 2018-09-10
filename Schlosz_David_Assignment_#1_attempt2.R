#Create x,y,z
x<-c(5,10,15,20,25,30)
y<-c(-1,NA,75,3,5,8)
z<-c(5)

#Multiply x and y by z 
xz<-(x*z)
yz<-(y*z)
print(xz)
print(yz)

#Replace NA in vector y with 2.5
y<-ifelse(is.na(y),2.5,y)
y*z

#Import Assignment 1 data set using imnport function. View Assignment 1
View(Assignment_1)

#Print the first 10 sstate abbreviations
Assignment_1$State[1:10]

#Mean US murder rate
mean(Assignment_1$Murder)

#Median US murder rate
median(Assignment_1$Murder)

#Create data set of only NE states
NEStates<-subset(Assignment_1,State=="CT"|State=="ME"|State=="MA"|State=="NH"|State=="RI"|State=="VT")

#Mean murdervrate in NE
mean(NEStates$Murder)

#Mean Vcrime rate in US
Assignment_1$Vcrime<-as.numeric(as.character(Assignment_1$Vcrime));mean(Assignment_1$Vcrime, na.rm = T)
