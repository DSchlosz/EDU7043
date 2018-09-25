#Import Data
Rdata<-read.csv("https://raw.githubusercontent.com/DSchlosz/EDU7043/master/Faith%20Communities%20Today%20Survey%20(FACT)%202010%2C%20Assemblies%20of%20God.csv", stringsAsFactors = F)

View(Rdata)

#Look at structure of data
str(Rdata)
names(Rdata)
variable.names(Rdata)

#Mean
mean(Rdata$SUNAMSER)
mean(Rdata$SUNPMSER)
mean(Rdata$FRISERVE)
mean(Rdata$SATSERVE)
mean(Rdata$SERVICES)

#Median
median(Rdata$SUNAMSER)
median(Rdata$SUNPMSER)
median(Rdata$FRISERVE)
median(Rdata$SATSERVE)
median(Rdata$SERVICES)

#Mode
modeSA<-table(Rdata$SUNAMSER)
View(modeSA)
# mode is  1

modeSP<-table(Rdata$SUNPMSER)
View(modeSP)

modeF<-table(Rdata$FRISERVE)
View(modeF)

modeSat<-table(Rdata$SATSERVE)
View(modeSat)

modeAll<-table(Rdata$SERVICES)
View(modeAll)


#SD
sd(Rdata$SUNAMSER)

#Variance
var(Rdata$SUNAMSER)

#Range
range(Rdata$SUNAMSER)

#Histogram (with name change to X axis)
hist(Rdata$SUNAMSER, xlab = "Sunday Morning Service")

#Table
table(Rdata$SUNAMSER)

#Stem Leaf
stem(Rdata$SUNAMSER)

#Bar plot
barplot(Rdata$SUNAMSER)

#Box plot
boxplot(Rdata$SUNAMSER)
