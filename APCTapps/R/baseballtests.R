
#install.packages("Lahman")
library(Lahman)
library(lubridate)

head(Pitching)
head(Master)

Dat <- merge(Pitching, Master, by = c("playerID"))

# let's just look at pitchers for now.
# P <- yearID
# C <- debut year
# D <- finalGame year
# A <- P - C
# T <- D - P
# L <- D - C

Dat$P     <- Dat$yearID

Dat$debut <- decimal_date(as.Date(Dat$debut))
Dat$C     <- floor(Dat$debut)
Dat$finalGame <- decimal_date(as.Date(Dat$finalGame))
Dat$D     <- floor(Dat$finalGame)
Dat$A     <- Dat$P - Dat$C
Dat$T     <- Dat$D - Dat$P
Dat$L     <- Dat$D - Dat$C

# now get demographic time measures set up (lwoercase)
Dat$d <- decimal_date(as.Date(Dat$deathDate))
Dat$c <- decimal_date(as.Date(Dat$birthDate))
Dat$l <- Dat$d - Dat$c
Dat$a <- Dat$P - Dat$c

# what age are the not-yet-dead people?
Dat$AgeEnd    <- ifelse(is.na(Dat$deathDate), Dat$finalGame - Dat$c, NA)
max(Dat$deathDate, na.rm=TRUE)

#--------------------------------
# curious: lexis plot of real lives
years <- 1871:2016

plot(NULL, type = "n", xlim = c(1871, 2016), ylim = c(0,110),asp=1)
segments(Dat$c, 0, Dat$d, Dat$l, col = "#00000005")
# living people
segments(Dat$c, 0, Dat$finalGame, Dat$AgeEnd, col = "#00CC0015")



