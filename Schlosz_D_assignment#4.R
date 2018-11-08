#Importing Data (using GSS)
gss<-read.csv("https://raw.githubusercontent.com/DSchlosz/EDU7043/master/GSS2.csv", stringsAsFactors = F)
names(gss)
str(gss)

# Setting up dataset i would like to work with by pulling variables: SEX, AGE, RACE, REBORN, SPKHOMO, COLHOMO
gd<-gss[ c(1:2867), c(69,46,70,317,145,144)]

# Removing the no answer and/or not applicable variables
gd$REBORN<-ifelse(gd$REBORN==8 | gd$REBORN==9 | gd$REBORN==0, NA, gd$REBORN )
gd$COLHOMO<-ifelse(gd$COLHOMO==8 | gd$COLHOMO==9 | gd$COLHOMO==0, NA, gd$COLHOMO )
gd$SPKHOMO<-ifelse(gd$SPKHOMO==8 | gd$SPKHOMO==9 | gd$SPKHOMO==0, NA, gd$SPKHOMO )
gd$AGE<-ifelse(gd$AGE==99 | gd$AGE==98, NA, gd$AGE)

gd1<-na.omit(gd)

# Creating relevant subsets
w<-subset(gd1, gd1$RACE==1)
b<-subset(gd1, gd1$RACE==2)
o<-subset(gd1, gd1$RACE==3)
m<-subset(gd1, gd1$SEX==1)
f<-subset(gd1, gd1$SEX==2)


# Subset of REBORN
allr<-subset(gd1, gd1$REBORN==1)
wr<-subset(gd1, gd1$RACE==1 | gd1$REBORN==1)
br<-subset(gd1, gd1$RACE==2 | gd1$REBORN==1)
or<-subset(gd1, gd1$RACE==3 | gd1$REBORN==1)
mr<-subset(gd1, gd1$SEX==1 | gd1$REBORN==1)
fr<-subset(gd1, gd1$SEX==2 | gd1$REBORN==1)

# subset of not REBORN
allnr<-subset(gd1, gd1$REBORN==2)
wnr<-subset(gd1, gd1$RACE==1 | gd1$REBORN==2)
bnr<-subset(gd1, gd1$RACE==2 | gd1$REBORN==2)
onr<-subset(gd1, gd1$RACE==3 | gd1$REBORN==2)
mnr<-subset(gd1, gd1$SEX==1 | gd1$REBORN==2)
fnr<-subset(gd1, gd1$SEX==2 | gd1$REBORN==2)


# Extracting central tendency and variance indicators

#All
summary(gd1$COLHOMO)
summary(w$COLHOMO)
summary(b$COLHOMO)
summary(o$COLHOMO)
summary(m$COLHOMO)
summary(f$COLHOMO)

sd(gd1$COLHOMO)
sd(w$COLHOMO)
sd(b$COLHOMO)
sd(o$COLHOMO)
sd(m$COLHOMO)
sd(f$COLHOMO)

#Reborn
summary(wr$COLHOMO)
summary(br$COLHOMO)
summary(or$COLHOMO)
summary(mr$COLHOMO)
summary(fr$COLHOMO)

#Not Reborn
summary(wnr$COLHOMO)
summary(bnr$COLHOMO)
summary(onr$COLHOMO)
summary(mnr$COLHOMO)
summary(fnr$COLHOMO)


#SD
sd(wr$COLHOMO)
sd(br$COLHOMO)
sd(or$COLHOMO)
sd(mr$COLHOMO)
sd(fr$COLHOMO)
sd(age1r$COLHOMO)
sd(age2r$COLHOMO)
sd(age3r$COLHOMO)
sd(age4r$COLHOMO)

sd(wnr$COLHOMO)
sd(bnr$COLHOMO)
sd(onr$COLHOMO)
sd(mnr$COLHOMO)
sd(fnr$COLHOMO)
sd(age1nr$COLHOMO)
sd(age2nr$COLHOMO)
sd(age3nr$COLHOMO)
sd(age4nr$COLHOMO)

#other
ry<-subset(gd1, gd1$REBORN==1)
rn<-subset(gd1, gd1$REBORN==2)
summary(rn$COLHOMO)
summary(ry$COLHOMO)
sd(rn$COLHOMO)
sd(ry$COLHOMO)

#Ttest for difference between reborn and not reborn opinion of homos teaching
t.test(ry$COLHOMO, rn$COLHOMO)

#setting up anova for examinining RACE diff opinion on homo teaching (not using REBORN as a factor)
cohomrace<-aov(gd1$COLHOMO ~ as.factor(gd1$RACE)) 
summary(cohomrace) #inidcates that there is a significant difference 
TukeyHSD(cohomrace) # shows that the significant differences are between nonwhites and whites

rcohomrace<-aov(allr$COLHOMO ~ as.factor(allr$RACE)) 
summary(rcohomrace)
TukeyHSD(rcohomrace)

#Running an ANOVA for Non-reborn differences between races
nrcohomrace<-aov(allnr$COLHOMO ~ as.factor(allnr$RACE)) 
summary(nrcohomrace) # this indicates a significant difference of 0.0404
TukeyHSD(nrcohomrace) # however when I run Tukey - no significant difference is apparent

wilcox.test(ry$COLHOMO, rn$COLHOMO)
wilcox.test(wnr$COLHOMO, wr$COLHOMO)
wilcox.test(mnr$COLHOMO, mr$COLHOMO)
wilcox.test(bnr$COLHOMO, br$COLHOMO)
wilcox.test(onr$COLHOMO, or$COLHOMO)
wilcox.test(fnr$COLHOMO, fr$COLHOMO)

wilcox.test(w$COLHOMO, w$COLHOMO)
wilcox.test(b$COLHOMO, b$COLHOMO)
wilcox.test(o$COLHOMO, o$COLHOMO)
wilcox.test(m$COLHOMO, m$COLHOMO)
wilcox.test(f$COLHOMO, f$COLHOMO)
wilcocx.test(gd1$COLHOMO, gd1$COLHOMO)
wilcox.test(m$COLHOMO, f$COLHOMO)

allpop<-aov(gd1$COLHOMO ~ as.factor(gd1$RACE)) 
summary(allpop) # this indicates a significant difference of 0.0404
TukeyHSD(allpop)

summary(allpop)
summary(gd1)
