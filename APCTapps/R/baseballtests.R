#install.packages("Lahman")
library(Lahman)
library(lubridate)
library(reshape2)
library(RColorBrewer)
library(dplyr)

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

head(Dat)


Dat$P           <- Dat$yearID
Dat$debut   	<- decimal_date(as.Date(Dat$debut))
Dat$C     		<- floor(Dat$debut)
Dat$finalGame 	<- decimal_date(as.Date(Dat$finalGame))
Dat$D     		<- floor(Dat$finalGame)
Dat$A     		<- Dat$P - Dat$C
Dat$T     		<- Dat$D - Dat$P
Dat$L     		<- Dat$D - Dat$C



# PCD, debut, retire. n = 5

# now get demographic time measures set up (lwoercase)
Dat$d 			<- decimal_date(as.Date(Dat$deathDate))
Dat$c 			<- decimal_date(as.Date(Dat$birthDate))

# if missing death day but age > 110, I doubt it, 
# assign Right side as death day
Right           <- max(Dat$finalGame, na.rm = TRUE)
Dat$d[with(Dat, (Right - c) > 110 & is.na(d))] <- Right

# missing birthday, assume 20 when debut
Dat$c[is.na(Dat$c)] <- Dat$debut[is.na(Dat$c)] - 20

Dat$l 			<- Dat$d - Dat$c
Dat$a 			<- Dat$P - Dat$c
Dat$t 			<- Dat$d - Dat$P
# round down for both, for grouping
Dat$tt          <- floor(Dat$t)
Dat$aa          <- floor(Dat$a)
# ---------------------------------------------------

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

#function for weighted mean
wmean <- function(x, w){
	sum(x * w, na.rm=TRUE) / sum(w, na.rm=TRUE)
}

# color ramp
YlGn <- colorRampPalette(brewer.pal(9, "YlGn"), space = "Lab") #all color ramp

#hist(Dat$c)
#hist(Dat$l[Dat$c < 1900])
#hist(Dat$l[Dat$c >=  1900 & Dat$c < 1920])
#hist(Dat$ERA[Dat$ERA< 10])
Dat$ERA2 	<- round(Dat$ERA * 5) / 5 # round to nearest .2, for grouping
eras 		<- sort(unique(Dat$ERA2))
eras 		<- eras[eras < 10]
# ----------------------------------
# seasons played by era, count
ind <- !is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA2 < 10
mat 		<- acast(Dat[ind,], ERA2~A, length)
brks        <- seq(0, 500, by = 50)
image(0:27, eras, t(mat), 
		xlab = "Seasons played", 
		ylab = "ERA", 
		col = YlGn(length(brks) - 1), 
		breaks = brks)

# ----------------------------------
# seasons until retirement, count?
ind <- !is.na(Dat$ERA) & !is.na(Dat$T) & Dat$ERA2 < 10
mat 		<- acast(Dat[ind,], ERA2~T, length)
brks        <- seq(0, 500, by = 50)
image(0:27, eras, t(mat[, as.character(0:27)]), 
		xlab = "Seasons left", 
		ylab = "ERA", 
		col = YlGn(length(brks)-1), 
		breaks = brks)
# note: same break at 4.2


#range(Dat$a, na.rm=TRUE)
ind <- !is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA < 10 & Dat$ERA > 0
mat <- acast(Dat[ind,], aa~A, value.var = "ERA", mean, na.rm = TRUE)
mat[mat>6] <- 6
brks <- seq(0,6,by=.25)
image(as.integer(colnames(mat)),
		as.integer(rownames(mat)),
		t(mat), 
		asp = 1, 
		breaks = brks,
		col = YlGn(length(brks) - 1),
		xlab = "Seasons played",
		ylab = "Age")

# and Age by seasons left? (lifelines move left and up)
ind <- !is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA < 10 & Dat$ERA > 0
mat <- acast(Dat[ind,], aa~T, value.var = "ERA", mean, na.rm = TRUE)
mat[mat > 6] <- 6
mat <- mat[, as.character(0:27)]
brks <- seq(0, 6, by = .25)
image(as.integer(colnames(mat)),
		as.integer(rownames(mat)),
		t(mat), 
		asp = 1, 
		breaks = brks,
		col = YlGn(length(brks) - 1),
		xlab = "Seasons left",
		ylab = "Age")



# ------------------------------------
# TAL, compressed
sel <- as.character(0:27)
ind <- !is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA < 10 & Dat$ERA > 0
TAL1 <- acast(Dat[ind,], 
		T~A, value.var = "ERA", mean, na.rm = TRUE)
TAL1 <- TAL1[sel,sel]
TAL1[TAL1 > 6] <- 6
image(as.integer(colnames(TAL1)),
		as.integer(rownames(TAL1)),
		t(TAL1), 
		asp = 1, 
		xlab = "Seasons played", 
		ylab = "Seasons remaining",
		breaks = brks,
		col = YlGn(length(brks) - 1))

# -----------------------
#example for weighted mean (ERA weighted by Batters faced by Pitcher)
TAL2 <- Dat %>% group_by(T, A) %>% 
		summarise(ERAw = wmean(ERA, BFP)) %>%
		acast(T ~ A, value.var = "ERAw")
TAL2 <- TAL2[sel,sel]
TAL2[TAL2 > 6] <- 6
image(as.integer(colnames(TAL2)),
		as.integer(rownames(TAL2)),
		t(TAL2), 
		asp = 1, 
		xlab = "Seasons played", 
		ylab = "Seasons remaining",
		col = YlGn(length(brks) - 1),
		breaks = brks)
# -----------------------
#example for weighted mean (ERA weighted by Batters faced by Pitcher)

TAL3 <- Dat %>% group_by(tt, aa) %>% 
		summarise(ERAw = wmean(ERA, BFP)) %>%
		acast(tt ~ aa, value.var = "ERAw")
#TAL3 <- TAL2[sel,sel]
TAL3[TAL3 > 6] <- 6
rownames(TAL3)
TAL3 <- TAL3[as.integer(rownames(TAL3))<81, as.character(15:49)]
TAL3 <- TAL3[-nrow(TAL3),]
image(as.integer(colnames(TAL3)),
		as.integer(rownames(TAL3)),
		t(TAL3), 
		asp = 1, 
		xlab = "Years lived", 
		ylab = "Years left",
		col = YlGn(length(brks) - 1),
		breaks = brks)



TAL1 <- TAL1[-nrow(TAL1),]
image(as.integer(colnames(TAL1)),
		as.integer(rownames(TAL1)),
		t(TAL1), asp = 1, xlab = "Seasons played", ylab = "Seasons remaining")


# note bathtun shape career arc, cool

# LCD, compressed
LCD1 <- acast(Dat[!is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA < 10 & Dat$ERA > 0,], L~C, value.var = "ERA", mean, na.rm=TRUE)
LCD1 <- LCD1[-nrow(LCD1),]
image(as.integer(colnames(LCD1)),as.integer(rownames(LCD1)),t(LCD1), asp = 1,xlab = "Debut year", ylab = "Career Length")
?topo.colors
# TPD, compressed
TPD1 <- acast(Dat[!is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA < 10 & Dat$ERA > 0,], T~P, value.var = "ERA", mean, na.rm=TRUE)
TPD1 <- TPD1[-nrow(TPD1),]
image(as.integer(colnames(TPD1)),as.integer(rownames(TPD1)),t(TPD1), asp = 1,xlab = "Year", ylab = "Seasons remaining")

# APC, compressed
APC1 <- acast(Dat[!is.na(Dat$ERA) & !is.na(Dat$A) & Dat$ERA < 10 & Dat$ERA > 0,], A~P, value.var = "ERA", mean, na.rm=TRUE)
APC1 <- APC1[-nrow(APC1),]
image(as.integer(colnames(APC1)),as.integer(rownames(APC1)),t(APC1), asp = 1,xlab = "Year", ylab = "Seasons played")


# 1. idea: check different cohorts
head(Dat)
hist(sort(Dat$birthYear))
unique(sort(Dat$birthYear))

#divide into different cohorts (limits are ca. 20years)
#example for cohort 1835-1859
mlb.1835.1859<-Dat[Dat$birthYear<=1859,]
dim(mlb.1835.1859)

TAL.1835.1859 <- acast(mlb.1835.1859[!is.na(mlb.1835.1859$ERA) & !is.na(mlb.1835.1859$A) & mlb.1835.1859$ERA < 10 & mlb.1835.1859$ERA > 0,], T~A, value.var = "ERA", mean, na.rm=TRUE)
TAL.1835.1859 <- TAL.1835.1859[-nrow(TAL.1835.1859),]
image(as.integer(colnames(TAL.1835.1859)),as.integer(rownames(TAL.1835.1859)),t(TAL.1835.1859), asp = 1, xlab = "Seasons played", ylab = "Seasons remaining",col=topo.colors(50))

#example for cohort 1860-1884
mlb.1860.1884<-Dat[Dat$birthYear>=1860 & Dat$birthYear<=1884,]
dim(mlb.1860.1884)
TAL.1860.1884 <- acast(mlb.1860.1884[!is.na(mlb.1860.1884$ERA) & !is.na(mlb.1860.1884$A) & mlb.1860.1884$ERA < 10 & mlb.1860.1884$ERA > 0,], T~A, value.var = "ERA", mean, na.rm=TRUE)
TAL.1860.1884 <- TAL.1860.1884[-nrow(TAL.1860.1884),]
image(as.integer(colnames(TAL.1860.1884)),as.integer(rownames(TAL.1860.1884)),t(TAL.1860.1884), asp = 1, xlab = "Seasons played", ylab = "Seasons remaining",col=topo.colors(50))


#example for cohort 1885-1909
mlb.1885.1909<-Dat[Dat$birthYear>=1885 & Dat$birthYear<=1909,]
dim(mlb.1885.1909)
TAL.1885.1909 <- acast(mlb.1885.1909[!is.na(mlb.1885.1909$ERA) & !is.na(mlb.1885.1909$A) & mlb.1885.1909$ERA < 10 & mlb.1885.1909$ERA > 0,], T~A, value.var = "ERA", mean, na.rm=TRUE)
TAL.1885.1909 <- TAL.1885.1909[-nrow(TAL.1885.1909),]
image(as.integer(colnames(TAL.1885.1909)),as.integer(rownames(TAL.1885.1909)),t(TAL.1885.1909), asp = 1, xlab = "Seasons played", ylab = "Seasons remaining",col=topo.colors(50))

#example for cohort 1910-1934
mlb.1910.1934<-Dat[Dat$birthYear>=1910 & Dat$birthYear<=1934,]
dim(mlb.1910.1934)
TAL.1910.1934 <- acast(mlb.1910.1934[!is.na(mlb.1910.1934$ERA) & !is.na(mlb.1910.1934$A) & mlb.1910.1934$ERA < 10 & mlb.1910.1934$ERA > 0,], T~A, value.var = "ERA", mean, na.rm=TRUE)
TAL.1910.1934 <- TAL.1910.1934[-nrow(TAL.1910.1934),]
image(as.integer(colnames(TAL.1910.1934)),as.integer(rownames(TAL.1910.1934)),t(TAL.1910.1934), asp = 1, xlab = "Seasons played", ylab = "Seasons remaining",col=topo.colors(50))


#example for cohort 1935-1959
mlb.1935.1959<-Dat[Dat$birthYear>=1935 & Dat$birthYear<=1959,]
dim(mlb.1935.1959)
TAL.1935.1959 <- acast(mlb.1935.1959[!is.na(mlb.1935.1959$ERA) & !is.na(mlb.1935.1959$A) & mlb.1935.1959$ERA < 10 & mlb.1935.1959$ERA > 0,], T~A, value.var = "ERA", mean, na.rm=TRUE)
TAL.1935.1959 <- TAL.1935.1959[-nrow(TAL.1935.1959),]
image(as.integer(colnames(TAL.1935.1959)),as.integer(rownames(TAL.1935.1959)),t(TAL.1935.1959), asp = 1, xlab = "Seasons played", ylab = "Seasons remaining",col=topo.colors(50))

#example for cohort 1960-1984
mlb.1960.1984<-Dat[Dat$birthYear>=1960 & Dat$birthYear<=1984,]
dim(mlb.1960.1984)
TAL.1960.1984 <- acast(mlb.1960.1984[!is.na(mlb.1960.1984$ERA) & !is.na(mlb.1960.1984$A) & mlb.1960.1984$ERA < 10 & mlb.1960.1984$ERA > 0,], T~A, value.var = "ERA", mean, na.rm=TRUE)
TAL.1960.1984 <- TAL.1960.1984[-nrow(TAL.1960.1984),]
image(as.integer(colnames(TAL.1960.1984)),as.integer(rownames(TAL.1960.1984)),t(TAL.1960.1984), asp = 1, xlab = "Seasons played", ylab = "Seasons remaining",col=topo.colors(50))

#example for cohort 1985-1995
mlb.1985.plus<-Dat[Dat$birthYear>=1985,]
dim(mlb.1985.plus)
TAL.1985.plus <- acast(mlb.1985.plus[!is.na(mlb.1985.plus$ERA) & !is.na(mlb.1985.plus$A) & mlb.1985.plus$ERA < 10 & mlb.1985.plus$ERA > 0,], T~A, value.var = "ERA", mean, na.rm=TRUE)
TAL.1985.plus <- TAL.1985.plus[-nrow(TAL.1985.plus),]
image(as.integer(colnames(TAL.1985.plus)),as.integer(rownames(TAL.1985.plus)),t(TAL.1985.plus), asp = 1, xlab = "Seasons played", ylab = "Seasons remaining",col=topo.colors(50))


#further ideas for next week
#write function to clear the code 





#difference between right or left handed?
table(Dat$throws) 

#12268 left handed pitcher
#31533 right handed pitcher

right.handed<-Dat[Dat$throws=="R",]
left.handed<-Dat[Dat$throws=="L",]

#first for right handed pitcher

right.weighted <- right.handed %>% group_by(T, A) %>% 
		summarise(ERAw = wmean(ERA, BFP)) %>%
		acast(T ~ A, value.var = "ERAw")
right.weighted <- right.weighted[-nrow(right.weighted),-ncol(right.weighted)]
right.weighted[right.weighted > 5] <- 5
image(as.integer(colnames(right.weighted)),
		as.integer(rownames(right.weighted)),
		t(right.weighted), 
		asp = 1, 
		xlab = "Seasons played", 
		ylab = "Seasons remaining",
		col = YlGn(50))





#left handed pitcher

left.weighted <- left.handed %>% group_by(T, A) %>% 
		summarise(ERAw = wmean(ERA, BFP)) %>%
		acast(T ~ A, value.var = "ERAw")
left.weighted <- left.weighted[-nrow(left.weighted),-ncol(left.weighted)]
left.weighted[left.weighted > 5] <- 5
image(as.integer(colnames(left.weighted)),
		as.integer(rownames(left.weighted)),
		t(left.weighted), 
		asp = 1, 
		xlab = "Seasons played", 
		ylab = "Seasons remaining",
		col = YlGn(50))



# maybe left handed pitcher get suck more at the on their career than right handed pitcher

# just some other comparisons for fun

# average career duration
mean(right.handed$L,na.rm=TRUE)   #8.489741 years
mean(left.handed$L,na.rm=TRUE)    #9.009864 years -> left handed pitcher play a little bit longer

#average age at first MLB game
mean(right.handed$a,na.rm=TRUE)   #27.33409 years
mean(left.handed$a,na.rm=TRUE)    #27.36214 years -> no difference

#average life span
mean(right.handed$l,na.rm=TRUE)   #68.11335 years
mean(left.handed$l,na.rm=TRUE)    #68.94831 years -> left handed pitcher live almost one year longer









#difference between height of the pitcher?

#http://www.espn.com/mlb/story/_/id/12751620/for-major-league-baseball-pitchers-bigger-better
#http://www.axonpotential.com/for-baseball-pitchers-height-does-matter/
#		https://constructal.files.wordpress.com/2013/07/dne146_baseball.pdf




#difference of the career-starting age?


#difference by club (maybe Yankee pitchers always better?) -> maybe allows control of scouting
#specify for each club and take a look at historical development of the pitcher
#check for first year played 


#do the same for strike outs (simple measure I know... )


