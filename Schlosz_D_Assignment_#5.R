#import data - GSS
dat<-read.csv(file = "https://raw.githubusercontent.com/DSchlosz/EDU7043/master/GSS2.csv", stringsAsFactors=F)
names(dat)
str(dat)

#Isolate variables for use: SEX, AGE, RACE, COLHOMO
gd<-dat[ c(1:2867), c(69,46,70,145)]

# Removing the no answer and/or not applicable variables
gd$COLHOMO<-ifelse(gd$COLHOMO==8 | gd$COLHOMO==9 | gd$COLHOMO==0, NA, gd$COLHOMO )
gd$AGE<-ifelse(gd$AGE==99 | gd$AGE==98, NA, gd$AGE)

gd1<-na.omit(gd)

# Creating relevant subsets
w<-subset(gd1, gd1$RACE==1)
b<-subset(gd1, gd1$RACE==2)
o<-subset(gd1, gd1$RACE==3)
m<-subset(gd1, gd1$SEX==1)
f<-subset(gd1, gd1$SEX==2)

#Check the data
table(w$COLHOMO)
table(b$COLHOMO)
table(o$COLHOMO)
table(m$COLHOMO)
table(f$COLHOMO)


summary(w$COLHOMO)
summary(b$COLHOMO)
summary(o$COLHOMO)
summary(m$COLHOMO)
summary(f$COLHOMO)

sd(w$COLHOMO)
sd(b$COLHOMO)
sd(o$COLHOMO)
sd(m$COLHOMO)
sd(f$COLHOMO)


table(gd1$COLHOMO, gd1$RACE)
ftable(gd1$COLHOMO, gd1$RACE)


#Cross Tabulation
library(gmodels)
CrossTable(gd1$COLHOMO, gd1$RACE, prop.chisq =F)
CrossTable(gd1$COLHOMO, gd1$RACE, expected=T)

CrossTable(gd1$COLHOMO, gd1$SEX, prop.chisq =F)
CrossTable(gd1$COLHOMO, gd1$SEX, expected=T)

#Ttest for difference between male and female opinion of homos teaching
t.test(m$COLHOMO, f$COLHOMO)

#setting up anova for examinining RACE diff opinion on homo teaching 
cohomrace<-aov(gd1$COLHOMO ~ as.factor(gd1$RACE)) 
summary(cohomrace) #inidcates that there is a significant difference 
TukeyHSD(cohomrace) # shows that the significant differences are between nonwhites and whites

rcohomrace<-aov(allr$COLHOMO ~ as.factor(allr$RACE)) 
summary(rcohomrace)
TukeyHSD(rcohomrace)
