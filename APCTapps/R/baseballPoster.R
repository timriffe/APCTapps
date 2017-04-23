

#install.packages("Lahman")
library(Lahman)
library(lubridate)
library(reshape2)
library(RColorBrewer)
library(dplyr)
source("/home/tim/git/APCTapps/APCTapps/R/Functions.R")


batting <- battingStats()
Dat <- merge(batting, Master, all.x = TRUE)

# need funtion to tell me all triad subidentities!
triads <- n_triads(5)
# now need translation to something understandable!

# clean birthdays:
# remove missing year
Dat 									<- Dat[!is.na(Dat$birthYear), ]
# midpoint missing months or days
Dat$birthMonth[is.na(Dat$birthMonth)] 	<- 6
Dat$birthDay[is.na(Dat$birthDay)] 		<- 15
# make date string, assign where date missing
birthdays 								<- paste(Dat$birthYear,Dat$birthMonth,Dat$birthDay,sep="-")
bdmissing 								<- is.na(Dat$birthDate)
Dat$birthDate[bdmissing]                <- birthdays[bdmissing]

# need to know career info
Dat <- Dat[!is.na(Dat$debut), ]

# Events
Dat$p1 <- decimal_date(as.Date(Dat$birthDate))   # time birth
Dat$p2 <- decimal_date(as.Date(Dat$debut))       # debut
Dat$p3 <- Dat$yearID + .5                        # period
Dat$p4 <- decimal_date(as.Date(Dat$finalGame))   # retired
Dat$p5 <- decimal_date(as.Date(Dat$deathDate))   # time death

# Durations, allowing for NAs
Dat$d1   <- Dat$p2 - Dat$p1  # age at debut
Dat$d2   <- Dat$p3 - Dat$p1  # age
Dat$d3   <- Dat$p3 - Dat$p2  # time since (until) debut
Dat$d4   <- Dat$p4 - Dat$p1  # age retired
Dat$d5   <- Dat$p4 - Dat$p2  # career length
Dat$d6   <- Dat$p4 - Dat$p3  # time to retire
Dat$d7   <- Dat$p5 - Dat$p1  # lifespan
Dat$d8   <- Dat$p5 - Dat$p2  # life left at debut
Dat$d9   <- Dat$p5 - Dat$p3  # time to death
Dat$d10  <- Dat$p5 - Dat$p4  # length of retirement

# remove d7 > 100
Dat <- Dat[Dat$d7 <= 105 | is.na(Dat$d7), ]


labels <- c("Birth cohort","Debut year","Period", "Year retired","Year of death",
		"age at debut", 
		"age","time since debut", 
		"age retired","career length", "time to retire",
		 "lifespan", "life left at debut", "time to death","length of retirement")
names(labels) <- c(paste0("p",1:5),paste0("d",1:10))


# some cleaning
Dat 	 <- Dat[Dat$d4 > 15 | is.na(Dat$d4), ]
Dat 	 <- Dat[Dat$d5 >= 0 | is.na(Dat$d5), ]
Dat 	 <- Dat[Dat$d7 <= 101 | is.na(Dat$d7), ]

ramp <- colorRampPalette(RColorBrewer::brewer.pal(9,"YlGn"),space="Lab")

#make function to take first 2 elements of each triad, plot BA average
SurfGeneric <- function(Dat, x="d1",y="p1",stat = "BA", agg = mean,contour=FALSE,labels,ramp,smoothit=TRUE,...){
	Dati     <- Dat
	
	keep     <- !is.na(Dati[,x]) & !is.na(Dati[,y])
	Dati     <- Dati[keep, ]
	Dati[,x] <- floor(Dati[,x] )
	Dati[,y] <- floor(Dati[,y] )
	#any(is.na(Dati[,y]))
	#denom   <- acast(Dat, as.formula(paste0(y,"~",x)),length,value.var="yearID",fill = NA_real_)
	
	Surf    <- acast(Dati, as.formula(paste0(y,"~",x)),agg,value.var=stat,fill=NA_real_,na.rm=TRUE)
	xat     <- as.integer(colnames(Surf))
	yat     <- as.integer(rownames(Surf))
	if (smoothit){
	Surfsm <- image.smooth(Surf)$z
	Surfsm[is.na(Surf)] <- NA
	Surf <- Surfsm
    }
	
	Nplayers <- length(unique(Dati$playerID))
	Nobs     <- nrow(Dati)
	image(xat,yat,t(Surf),asp=1, xlab = labels[x], ylab = labels[y], 
			main = paste(as.character(substitute(agg)),stat,labels[y],"by",labels[x]),
			sub = paste0(Nobs, " observations of ", Nplayers, " players"),col=ramp)
	if(contour)	contour(xat,yat,t(Surf),add=TRUE)
}

# BA: battive avg
# PA: plate appearances
# TB: total bases
# SlugPct: slugging pct
# OBP: on base pct
# OPS: on base pct + slugging
# BABIP: batting avg on balls in play
cv <- function(x,...){sd(x,...)/mean(x,...)}

stats <- c("BA","PA","TB","SlugPct","OBP","OPS","BABIP")

# sd
lapply(stats, function(stat, Dat){
			path <- paste0("/home/tim/git/APCTapps/APCTapps/Figures/triads",stat,"sd.pdf")
			pdf(path)
			for (i in 1:length(triads)){
				SurfGeneric(Dat=Dat, 
						x = triads[[i]][1], 
						y = triads[[i]][2], 
						stat = stat, 
						agg = cv, 
						labels = labels, 
						na.rm=TRUE,
						ramp=rev(ramp(20)),
						contour=TRUE)
			}
			dev.off()
		}, Dat=Dat)
# mean
lapply(stats, function(stat, Dat){
			path <- paste0("/home/tim/git/APCTapps/APCTapps/Figures/triads",stat,"mean.pdf")
			pdf(path)
			for (i in 1:length(triads)){
				SurfGeneric(Dat=Dat, 
						x = triads[[i]][1], 
						y = triads[[i]][2], 
						stat = stat, 
						agg = mean, 
						labels = labels, 
						na.rm=TRUE,
						ramp=rev(ramp(20)),
						contour=TRUE)
			}
			dev.off()
		}, Dat=Dat)
# CV
lapply(stats, function(stat, Dat){
			path <- paste0("/home/tim/git/APCTapps/APCTapps/Figures/triads",stat,"CV.pdf")
			pdf(path)
			for (i in 1:length(triads)){
				SurfGeneric(Dat=Dat, 
						x = triads[[i]][1], 
						y = triads[[i]][2], 
						stat = stat, 
						agg = cv, 
						labels = labels, 
						na.rm=TRUE,
						ramp=rev(ramp(20)),
						contour=TRUE)
			}
			dev.off()
		}, Dat=Dat)

# --------------------------------------------

# Monday TODO:
# make grid of surfaces, more artisanally



