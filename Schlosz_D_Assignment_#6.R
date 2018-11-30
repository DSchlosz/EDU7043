datam<-read.spss("https://raw.githubusercontent.com/DSchlosz/EDU7043/master/PRRI-RNS-August-2015-Survey.sav", to.data.frame = T)
names(datam)
str(datam)

# Setting up dataset i would like to work with by pulling variables
datm<-datam[ c(1:1331), c(19,30,82, 124, 133, 134, 135)]

#remove NA
datm<-na.omit(datm)

#remove "refused" from age
datm$age<-ifelse(datm$age=="refused", NA, datm$age)
datm1<-datm
datm1$q2g<-ifelse(datm1$q2g=="Don't know/Refused"  |  datm1$q2g=="Have not heard of",  NA, datm1$q2g)
datm1$q15a<-ifelse(datm1$q15a=="Don't know/Refused"  |  datm1$q15a=="Have not heard of",  NA, datm1$q15a)
datm1$q19a<-ifelse(datm1$q19a=="Don't know/Refused"  |  datm1$q19a=="Have not heard of",  NA, datm1$q19a)
datm1$q19b<-ifelse(datm1$q19b=="Don't know/Refused"  |  datm1$q19b=="Have not heard of",  NA, datm1$q19b)
datm1$q19c<-ifelse(datm1$q19c=="Don't know/Refused"  |  datm1$q19c=="Have not heard of",  NA, datm1$q19c)

datm1<-na.omit(datm1)

# Creating Christian and Non-Christian subsets
C<-subset(datm1, datm1$q2g==1  | datm1$q2g==2)
NC<-subset(datm1, datm1$q2g==3  | datm1$q2g==4)


table(datm1$q2g)
table(datm1$q15a)
table(datm1$q19a)
table(datm1$q19b)
table(datm1$q19c)

summary(datm1$q2g)
summary(datm1$q15a)
summary(datm1$q19a)
summary(datm1$q19b)
summary(datm1$q19c)


sd(datm1$q2g)
sd(datm1$q15a)
sd(datm1$q19a)
sd(datm1$q19b)
sd(datm1$q19c)


table(datm1$q2g, datm1$q15a)
table(datm1$q2g, datm1$q19a)
table(datm1$q2g, datm1$q19b)
table(datm1$q2g, datm1$q19c)


#Cross Tabulation
library(gmodels)


CrossTable(datm1$q2g, datm1$q15a, prop.chisq =F)
CrossTable(datm1$q2g, datm1$q15a, expected=T)

hist(datm1$q2g) #parametric
hist(datm1$q15a) #nonparametric
hist(datm1$q19a) #nonparametric
hist(datm1$q19b) #nonparametric
hist(datm1$q19c) #parametric


cor(datm1$q2g, datm1$q15a, method="pearson")
cor.test(datm1$q2g, datm1$q15a)

cor(datm1$q2g, datm1$q15a, method="spearman")
cor.test(datm1$q2g, datm1$q15a)

cor(datm1$q2g, datm1$q19a, method="spearman")
cor.test(datm1$q2g, datm1$q19a)
cor(datm1$q2g, datm1$q19b, method="spearman")
cor.test(datm1$q2g, datm1$q19b)
cor(datm1$q2g, datm1$q19c, method="pearson")
cor.test(datm1$q2g, datm1$q19c)
