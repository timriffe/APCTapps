
#install.packages("Lahman")
library(Lahman)
library(lubridate)
library(reshape2)
library(RColorBrewer)
library(dplyr)
source("/home/tim/git/APCTapps/APCTapps/R/Functions.R")

Dat             <- merge(Pitching, Master, by = c("playerID"))
#summary(Dat$stint)
#Dat$P           <- Dat$yearID
#Dat$debut   	<- decimal_date(as.Date(Dat$debut))
#Dat$C     		<- floor(Dat$debut)
#Dat$finalGame 	<- decimal_date(as.Date(Dat$finalGame))
#Dat$D     		<- floor(Dat$finalGame)
#Dat$A     		<- Dat$P - Dat$C
#Dat$T     		<- Dat$D - Dat$P
#Dat$L     		<- Dat$D - Dat$C

# need funtion to tell me all triad subidentities!
triads <- n_triads(5)
# now need translation to something understandable!

# clean birthdays:
# remove missing year
sum(is.na(Dat$birthYear))
Dat 									<- Dat[!is.na(Dat$birthYear), ]
# midpoint missing months or days
Dat$birthMonth[is.na(Dat$birthMonth)] 	<- 6
Dat$birthDay[is.na(Dat$birthDay)] 		<- 15
# make date string, assign where date missing
birthdays 								<- paste(Dat$birthYear,Dat$birthMonth,Dat$birthDay,sep="-")
bdmissing 								<- is.na(Dat$birthDate)

Dat$birthDate[bdmissing] <- birthdays[bdmissing]

# Events
Dat$p1 <- decimal_date(as.Date(Dat$birthDate))   # time birth
Dat$p2 <- decimal_date(as.Date(Dat$debut))       # debut
Dat$p3 <- Dat$yearID + .5                        # period
Dat$p4 <- decimal_date(as.Date(Dat$finalGame))   # retired
Dat$p5 <- decimal_date(as.Date(Dat$deathDate))   # time death

head(Dat[is.na(Dat$p1),])
# no birth year, throw out

# can do a bulk transform
P <- as.matrix(Dat[,c("p1","p2","p3","p4","p5")])
sum(is.na(Dat$birthMonth))
At <- GenerateTransformationMatrix(5)
# get durations
d <- t(At %*% t(P) )
head(P)
head(d)
At %*% t(P)

colSums(is.na(P))
