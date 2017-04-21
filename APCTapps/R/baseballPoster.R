
#install.packages("Lahman")
library(Lahman)
library(lubridate)
library(reshape2)
library(RColorBrewer)
library(dplyr)


Dat             <- merge(Pitching, Master, by = c("playerID"))
summary(Dat$stint)
Dat$P           <- Dat$yearID
Dat$debut   	<- decimal_date(as.Date(Dat$debut))
Dat$C     		<- floor(Dat$debut)
Dat$finalGame 	<- decimal_date(as.Date(Dat$finalGame))
Dat$D     		<- floor(Dat$finalGame)
Dat$A     		<- Dat$P - Dat$C
Dat$T     		<- Dat$D - Dat$P
Dat$L     		<- Dat$D - Dat$C

# need funtion to tell me all triad subidentities!
triads <- n_triads(5)
# now need translation to something understandable!

# or I just need to pick p...
head(Dat)
Dat$p3 <- Dat$yearID + .5
Dat$p1 <- decimal_date(as.Date(Dat$birthDate))
Dat$p2 <- decimal_date(as.Date(Dat$debut))
Dat$p4 <- decimal_date(as.Date(Dat$finalGame))
Dat$p5 <- decimal_date(as.Date(Dat$deathDate))

P <- as.matrix(Dat[,c("p1","p2","p3","p4","p5")])

At <- GenerateTransformationMatrix(5)
# get durations
d <- t(At %*% t(P) )


