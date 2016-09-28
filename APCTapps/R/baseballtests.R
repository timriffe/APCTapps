
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

Dat$debut 		<- decimal_date(as.Date(Dat$debut))
Dat$C     		<- floor(Dat$debut)
Dat$finalGame 	<- decimal_date(as.Date(Dat$finalGame))
Dat$D     		<- floor(Dat$finalGame)
Dat$A     		<- Dat$P - Dat$C
Dat$T     		<- Dat$D - Dat$P
Dat$L     		<- Dat$D - Dat$C

# now get demographic time measures set up (lwoercase)
Dat$d 			<- decimal_date(as.Date(Dat$deathDate))
Dat$c 			<- decimal_date(as.Date(Dat$birthDate))

# if missing death day but age > 110, I doubt it, 
# assign Right side as death day
Right           <- max(Dat$finalGame, na.rm = TRUE)
Dat$d[with(Dat, (Right - c) > 110 & is.na(d))] <- Right

# missing birthday, assume 20 when debut
Dat$c[is.na(Dat$c)] <- Dat$debut - 20

Dat$l 			<- Dat$d - Dat$c
Dat$a 			<- Dat$P - Dat$c

# what age are the not-yet-dead people?
head(Dat)

Dat$AgeTrunc    <- ifelse(is.na(Dat$d),Right - Dat$c,NA)

#--------------------------------
# curious: lexis plot of real lives
years <- 1871:2016

plot(NULL, type = "n", xlim = c(1871, 2016), ylim = c(0,110),asp=1)
segments(Dat$c, 0, Dat$d, Dat$l, col = "#00000005",lwd=.5)
# living people
segments(Dat$c, 0, Right, Dat$AgeTrunc, col = "#00BB0003",lwd=.5)


wmean <- function(x, w){
	sum(x * w, na.rm=TRUE) / sum(w, na.rm=TRUE)
}



#hist(Dat$c)
#hist(Dat$l[Dat$c < 1900])
#hist(Dat$l[Dat$c >=  1900 & Dat$c < 1920])
#hist(Dat$ERA[Dat$ERA< 10])
#Dat$ERA2 <- round(Dat$ERA * 5) / 5
#library(reshape2)
#eras <- sort(unique(Dat$ERA2))
#eras <- eras[eras<10]
#mat <- acast(Dat[!is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA2 < 10,], ERA2~A, length)
#image(0:27,eras,log(t(mat)), xlab = "Season", ylab = "ERA")

#mat[1, ] <- 0
#lines(0:27,apply(mat,2,wmean,x=eras+.1))
Dat$aa <- floor(Dat$a)
mat <- acast(Dat[!is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA < 10 & Dat$ERA > 0,], aa~A, value.var = "ERA", mean, na.rm=TRUE)
mat <- mat[-nrow(mat),]
image(as.integer(colnames(mat)),as.integer(rownames(mat)),t(mat), asp = 1)

# TAL, compressed
TAL1 <- acast(Dat[!is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA < 10 & Dat$ERA > 0,], T~A, value.var = "ERA", mean, na.rm=TRUE)
TAL1 <- TAL1[-nrow(TAL1),]
image(as.integer(colnames(TAL1)),as.integer(rownames(TAL1)),t(TAL1), asp = 1, xlab = "Seasons played", ylab = "Seasons remaining")
# note bathtun shape career arc, cool

# LCD, compressed
LCD1 <- acast(Dat[!is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA < 10 & Dat$ERA > 0,], L~C, value.var = "ERA", mean, na.rm=TRUE)
LCD1 <- LCD1[-nrow(LCD1),]
image(as.integer(colnames(LCD1)),as.integer(rownames(LCD1)),t(LCD1), asp = 1,xlab = "Debut year", ylab = "Career Length")

# TPD, compressed
TPD1 <- acast(Dat[!is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA < 10 & Dat$ERA > 0,], T~P, value.var = "ERA", mean, na.rm=TRUE)
TPD1 <- TPD1[-nrow(TPD1),]
image(as.integer(colnames(TPD1)),as.integer(rownames(TPD1)),t(TPD1), asp = 1,xlab = "Year", ylab = "Seasons remaining")

# APC, compressed
APC1 <- acast(Dat[!is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA < 10 & Dat$ERA > 0,], A~P, value.var = "ERA", mean, na.rm=TRUE)
APC1 <- APC1[-nrow(APC1),]
image(as.integer(colnames(APC1)),as.integer(rownames(APC1)),t(APC1), asp = 1,xlab = "Year", ylab = "Seasons played")




