
# This one is correct:
T2<-read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/tarantino/tarantino.csv", stringsAsFactors = F)

names(T2)  
variable.names(T2)


#This worked to give a table of just the movie name and words used
myvar2<-c("movie", "word")
t2a<-T2[myvar2]

print(myvar2)

#This works at getting it as a table 
table(t2a)
myvar2


print(myvar2)

# t2a as a table
table(t2a)

#this works to list the words i dont want
notwords<-c("ass", "asses", "asshead", "assholes", "bastard", "bastards", "bitch", "bitches", "bullshit", "chickenshit", "cockblockery", "cocksucker", "cunt", "cunts", "damn", "damned", "dickless", "dicks", "dumbass", "faggot", "fucked", "fucker", "fuckers", "fuckface", "fuckhead", "fucking", "fucks", "fuckup", "goddamn", "goddamned", "gook", "gooks", "horseshit", "horeshit", "jackass", "jap", "japs", "jew (verb)", "merde", "motherfucker", "motherfuckers", "motherfucking", "n-word ", "negro ", "pussy", "shithead", "shitless", "shitload", "shittiest", "shitting", "shitty", "slope", "slut", "squaw", "wetback")
notmovies<-c("Jackie Brown", "Kill Bill: Vol. 2", "Kill Bill: Vol. 1")

variable.names(t2a)

#This works
table(t2a, exclude = notmovies) 


#this is the table that i want to work with
table(t2a, exclude = notwords)




View(data)

#At this point - after hours of using the google - I could not figure out how to take dataset "t2a" and make it into what i wanted.
#So I created dataset "tara" in excel and imported it so i could work with it for the rest of the assignment.

apply(tara)

#this works:
variable.names(tara)
variable.names(t2a)

#this works:
mean(tara[["asshole"]])
mean(tara[["dick"]])
mean(tara[["fuck"]])
mean(tara[["hell"]])
mean(tara[["shit"]])

#This also works:
mean(tara$asshole)
mean(tara$dick)
mean(tara$fuck)
mean(tara$hell)
mean(tara$shit)


# this does NOT work
mode(tara[["asshole"]])
mode(tara[["dick"]])
mode(tara[["fuck"]])
mode(tara[["hell"]])
mode(tara[["shit"]])

# this works
median(tara[["asshole"]])
median(tara[["dick"]])
median(tara[["fuck"]])
median(tara[["hell"]])
median(tara[["shit"]])

#sd:
sd(tara$asshole)
sd(tara$dick)
sd(tara$fuck)
sd(tara$hell)
sd(tara$shit)

#var: All of these work except for "asshole".
var:
var(tara$asshole)
var(tara$dick)
var(tara$fuck)
var(tara$hell)
var(tara$shit)

#range: These all work
range(tara$asshole)
range(tara$dick)
range(tara$fuck)
range(tara$hell)
range(tara$shit)

#max-min: These all work
max(tara$asshole)-min(tara$asshole)
max(tara$dick)-min(tara$dick)
max(tara$fuck)-min(tara$fuck)
max(tara$hell)-min(tara$hell)
max(tara$shit)-min(tara$shit)

#table
table(tara, exclude = notwords)

#str
str(tara)

#Stem-leaf plot
stem(tara$asshole)
stem(tara$dick)
stem(tara$fuck)

stem(tara$hell)
stem(tara$shit)

#Histogram
hist(tara$asshole)
hist(tara$dick)
hist(tara$fuck)
hist(tara$shit)
hist(tara$hell)


