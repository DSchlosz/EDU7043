#Bring In Data
pal<-read.csv("https://raw.githubusercontent.com/DSchlosz/EDU7043/master/Portraits%20of%20American%20Life%20Study%2C%20Merged%20Dataset%2C%202006-2012.csv", stringsAsFactors = F)

#Look at my data
names(pal)
str(pal)

#I'm interested in porn usage by christians, sex before marriage by christians, sex outside of marriage, and speaking to gay people.
library(plyr)
pal<-rename(pal, c(AMA3W1="nosex", ARQ7W1="sexinm", MA_5W1="porn"))

#See if changes came through
names(pal)

# create a subset
Naughty<-pal[c(1:100), c(7, 58, 228)]

#Let's look more at these variables. Are there any na's? #How many?
table(is.na(Naughty$nosex))
table(is.na(Naughty$sexinm))
table(is.na(Naughty$porn))

#Remove NAs


#What is the central tendency of these variables?
summary(Naughty$porn)
summary(Naughty$nosex)
summary(Naughty$sexinm)

#Mode function:
mode<-function(x) {
  unique_val<-unique(x)
  counts<-vector()
  for(i in 1: length(unique_val)){
    counts[i]<- length(which(x==unique_val[i]))
  }
  position<-c(which(counts==max(counts)))
  if(length(unique_val)==length(x))
    mode_x<-'Mode does not exist'
  else
    mode_x<-unique_val[position]
  return(mode_x)
}

#Determine Mode
mode(Naughty$nosex)
mode(Naughty$sexinm)
mode(Naughty$porn)

#load from psych package
library(psych)

#variance and range
describe(Naughty$nosex)
describe(Naughty$sexinm)
describe(Naughty$porn)

var(Naughty$nosex)
var(Naughty$sexinm)
var(Naughty$porn)





#Are these variables normally distributed? What tools do we have to begin to answer this question?
hist(Naughty$porn)
hist(Naughty$nosex)
hist(Naughty$sexinm)

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
nosex<-subset(Naughty, !is.na(Naughty$nosex))
Dist_lines(nosex$nosex)

porn<-subset(Naughty, !is.na(Naughty$porn))
Dist_lines(porn$porn)

sexinm<-subset(Naughty, !is.na(Naughty$sexinm))
Dist_lines(sexinm$sexinm)




#run plot
qqnorm(porn$porn); qqline(porn$porn)
qqnorm(nosex$nosex); qqline(nosex$nosex)
qqnorm(sexinm$sexinm); qqline(sexinm$sexinm)
