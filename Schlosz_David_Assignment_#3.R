#Bring In Data
pal<-read.csv("https://raw.githubusercontent.com/DSchlosz/EDU7043/master/Portraits%20of%20American%20Life%20Study%2C%20Merged%20Dataset%2C%202006-2012.csv", stringsAsFactors = F)

#Look at my data
names(pal)
str(pal)

#I'm interested in porn usage by christians, sex before marriage by christians, sex outside of marriage, and speaking to gay people.
library(plyr)
pal<-rename(pal, c(AMA3W1="nosex", ARQ7W1="sexinm", IC_10W1="spkgay", MA_5W1="porn"))

#See if changes came through
names(pal)

#Let's look more at these variables. Are there any na's? #How many?
table(is.na(pal$nosex))
table(is.na(pal$sexinm))
table(is.na(pal$spkgay))
table(is.na(pal$porn))


#What is the central tendency of these variables?
summary(pal$porn)
summary(pal$nosex)
summary(pal$spkgay)
summary(pal$sexinm)

#Are these variables normally distributed? What tools do we have to begin to answer this question?
hist(pal$porn)
hist(pal$nosex)
hist(pal$spkgay)
hist(pal$sexinm)

#I'm Making a Fake Data Set that appoximates normality
ex<-rnorm(10000, mean=0, sd=1)
summary(ex)
h<-hist(ex)

xfit<-seq(min(ex), max(ex), length=40)
yfit<-dnorm(xfit, mean=mean(ex), sd=sd(ex))
yfit<-yfit*diff(h$mids[1:2])*length(ex)
lines(xfit, yfit)

#Let's Look at our two variables. I'm creating a function for the steps above. You don't have to do this, but it can be helpful.
Dist_lines<-function(x){
  h<-hist(x)
  xfit<-seq(min(x), max(x), length=40)
  yfit<-dnorm(xfit, mean=mean(x), sd=sd(x))
  yfit<-yfit*diff(h$mids[1:2])*length(x)
  lines(xfit, yfit)
  
}

#Test the Function
Dist_lines(ex)

#Function that I created does not like NAs, so let's remove them and create a new object
nosex<-subset(pal, !is.na(pal$nosex))
Dist_lines(nosex$nosex)

porn<-subset(pal, !is.na(pal$porn))
Dist_lines(porn$porn)


sexinm<-subset(pal, !is.na(pal$sexinm))
Dist_lines(sexinm$sexinm)


#Another way to think about normality
#Q-Q Plots - Let's try our fake data first
qqnorm(ex); qqline(ex)

#Now let's try our real data
qqnorm(porn$porn); qqline(porn$porn)
