

#install.packages("Lahman")
library(Lahman)
library(lubridate)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(fields)
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
SurfGeneric <- function(Dat, triad.i,stat = "BA", agg = mean,contour=FALSE,labels,ramp,smoothit=TRUE,i,plot=TRUE,pars=FALSE...){
	
	anyp <- any(grepl("p",triad.i))
	if (anyp){
		x <- rev(sort(triad.i[grepl("p",triad.i)]))[1]
		y <- triad.i[grepl("d",triad.i)]
	} else {
		x <- triad.i[1]
		y <- triad.i[3]
	}
	
	Dati     <- Dat
	
	keep     <- !is.na(Dati[,x]) & !is.na(Dati[,y])
	Dati     <- Dati[keep, ]
	Dati[,x] <- floor(Dati[,x] )
	Dati[,y] <- floor(Dati[,y] )
	#any(is.na(Dati[,y]))
	#denom   <- acast(Dat, as.formula(paste0(y,"~",x)),length,value.var="yearID",fill = NA_real_)
	
	Surf     <- acast(Dati, as.formula(paste0(y,"~",x)),agg,value.var=stat,fill=NA_real_,na.rm=TRUE,drop=FALSE)
	xat      <- as.integer(colnames(Surf))
	yat      <- as.integer(rownames(Surf))
	if (smoothit){
	Surfsm   <- image.smooth(Surf)$z
	Surfsm[is.na(Surf)] <- NA
	Surf     <- Surfsm
    }
	colnames(Surf) <- xat
	rownames(Surf) <- yat
	Nplayers <- length(unique(Dati$playerID))
	Nobs     <- nrow(Dati)
	if (plot){
	image(xat,yat,t(Surf),asp=1, xlab = labels[x], ylab = labels[y], 
			main = paste(as.character(substitute(agg)),stat,labels[y],
					"by",labels[x],"\ntriad nr",i,"\n(",labels[triad.i[2]],"in diag)"),
			sub = paste0(Nobs, " observations of ", Nplayers, " players"),col=ramp)
	if(contour)	contour(xat,yat,t(Surf),add=TRUE)
    }
	out <- list(Surf=Surf,xat=xat,yat=yat,Nobs=Nobs,Nplayers=Nplayers,x=x,y=y,triad=triad.i)
	if (pars){
		return(invisible(out))
	}
	
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

labels[triads[[12]]]
labels[triads[[14]]]
labels[triads[[16]]]

labels[triads[[12]]]
labels[triads[[14]]]
labels[triads[[16]]]


# sd
lapply(stats, function(stat, Dat){
			path <- paste0("/home/tim/git/APCTapps/APCTapps/Figures/BaseballTriads/triads",stat,"sd.pdf")
			pdf(path)
			for (i in 1:length(triads)){
				SurfGeneric(Dat=Dat, 
						triad.i = triads[[i]], 
						stat = stat, 
						agg = cv, 
						labels = labels, 
						na.rm=TRUE,
						ramp=rev(ramp(20)),
						contour=TRUE,i=i)
			}
			dev.off()
		}, Dat=Dat)
# mean
lapply(stats, function(stat, Dat){
			path <- paste0("/home/tim/git/APCTapps/APCTapps/Figures/BaseballTriads/triads",stat,"mean.pdf")
			pdf(path)
			for (i in 1:length(triads)){
				SurfGeneric(Dat=Dat, 
						triad.i = triads[[i]], 
						stat = stat, 
						agg = mean, 
						labels = labels, 
						na.rm=TRUE,
						ramp=rev(ramp(20)),
						contour=TRUE,i=i)
			}
			dev.off()
		}, Dat=Dat)
# CV
lapply(stats, function(stat, Dat){
			path <- paste0("/home/tim/git/APCTapps/APCTapps/Figures/BaseballTriads/triads",stat,"CV.pdf")
			pdf(path)
			for (i in 1:length(triads)){
				SurfGeneric(Dat=Dat, 
						triad.i = triads[[i]], 
						stat = stat, 
						agg = cv, 
						labels = labels, 
						na.rm=TRUE,
						ramp=rev(ramp(20)),
						contour=TRUE,i=i)
			}
			dev.off()
		}, Dat=Dat)

# --------------------------------------------

# Monday TODO:
# make grid of surfaces, more artisanally

labels[triads[[12]]]
labels[triads[[14]]]
labels[triads[[16]]]


# CV
labels[triads[[12]]]

CVBA1 <- SurfGeneric(Dat=Dat, triad.i = triads[[12]], 
		stat = "BA", agg = cv, labels = labels, 
		ramp=rev(ramp(20)),contour=TRUE,i=12,pars=TRUE)
ATLRefN(0:50)
Surf <- CVBA1$Surf
NAout <- row(Surf)-col(Surf)[,ncol(Surf):1] > 0
Surf[NAout] <- NA
breaks <- pretty(Surf,n=20)
pdf("/home/tim/git/APCTapps/APCTapps/Figures/CVBA1.pdf",width=6,height=6)
par(xaxs="i",yaxs="i",xpd=FALSE)
image(CVBA1$xat+.5,CVBA1$yat+.5,t(Surf),asp=1, 
		xlab = labels[CVBA1$x], ylab = labels[CVBA1$y],
		col=rev(ramp(length(breaks)-1)),breaks=breaks, 
		ylim= c(0,30), xlim=c(15,45), axes = FALSE,
		panel.first=list(
				polygon(x=c(15,45,15),y=c(0,0,30), border=NA,col=gray(.85)),
				ATLRefN(0:45,col1="white",col2="white",col3="white"),
				text(seq(15,45,5),0,seq(15,45,5),pos=1,xpd=TRUE),
				text(15,seq(0,30,5),seq(0,30,5),pos=2,xpd=TRUE)
))
contour(CVBA1$xat,CVBA1$yat,t(Surf),add=TRUE)
ATLRefN(0:45,col1="#FFFFFF40",col2="#FFFFFF40",col3="#FFFFFF40")
dev.off()

# limit age retired to 35



CVBA2 <- SurfGeneric(Dat=Dat, triad.i = triads[[14]], 
		stat = "BA", agg = cv, labels = labels, 
		ramp=rev(ramp(20)),contour=TRUE,i=14,plot = FALSE,pars=TRUE)
Surf <- CVBA2$Surf
NAout <- row(Surf)-col(Surf)[,ncol(Surf):1] > -5
Surf[NAout] <- NA
breaks <- pretty(Surf,n=20)
pdf("/home/tim/git/APCTapps/APCTapps/Figures/CVBA2.pdf",width=6,height=6)
par(xaxs="i",yaxs="i",xpd=FALSE)
image(CVBA2$xat+.5,CVBA2$yat+.5,t(Surf),asp=1, 
		xlab = labels[CVBA2$x], ylab = labels[CVBA2$y],
		col=rev(ramp(length(breaks)-1)),breaks=breaks, 
		ylim= c(0,35), xlim=c(15,50), axes = FALSE,
		panel.first=list(
				polygon(x=c(15,50,15),y=c(0,0,35), border=NA,col=gray(.85)),
				ATLRefN(0:50,col1="white",col2="white",col3="white",xpd=TRUE),
				text(seq(15,50,5),0,seq(15,50,5),pos=1,xpd=TRUE),
				text(15,seq(0,35,5),seq(0,35,5),pos=2,xpd=TRUE)
		))
contour(CVBA2$xat,CVBA2$yat,t(Surf),add=TRUE)
ATLRefN(0:50,col1="#FFFFFF40",col2="#FFFFFF40",col3="#FFFFFF40")
dev.off()
# limit age retired to 35

CVBA3 <- SurfGeneric(Dat=Dat, triad.i = triads[[16]], 
		stat = "BA", agg = cv, labels = labels, 
		ramp=rev(ramp(20)),contour=TRUE,i=16,plot = TRUE,pars=TRUE)
# limit career length to 33
Surf <- CVBA3$Surf
NAout <- row(Surf)-col(Surf)[,ncol(Surf):1] > -2
Surf[NAout] <- NA
breaks <- pretty(Surf,n=20)
pdf("/home/tim/git/APCTapps/APCTapps/Figures/CVBA3.pdf",width=6,height=6)
par(xaxs="i",yaxs="i",xpd=FALSE)
image(CVBA3$xat+1.5,CVBA3$yat+.5,t(Surf),asp=1, 
		xlab = labels[CVBA3$x], ylab = labels[CVBA3$y],
		col=rev(ramp(length(breaks)-1)),breaks=breaks, 
		ylim= c(0,30), xlim=c(0,30), axes = FALSE,
		panel.first=list(
				polygon(x=c(0,30,0),y=c(0,0,30), border=NA,col=gray(.85)),
				ATLRefN(0:30,col1="white",col2="white",col3="white",xpd=TRUE),
				text(seq(0,30,5),0,seq(0,30,5),pos=1,xpd=TRUE),
				text(0,seq(0,30,5),seq(0,30,5),pos=2,xpd=TRUE)
		))
contour(CVBA3$xat+1.5,CVBA3$yat,t(Surf),add=TRUE)
ATLRefN(0:30,col1="#FFFFFF40",col2="#FFFFFF40",col3="#FFFFFF40")
dev.off()






meanBA1 <- SurfGeneric(Dat=Dat, triad.i = triads[[12]], 
		stat = "BA", agg = mean, labels = labels, 
		ramp=rev(ramp(20)),contour=TRUE,i=12,plot = FALSE,pars=TRUE)
# limit age retired to 35
Surf <- meanBA1$Surf
NAout <- row(Surf)-col(Surf)[,ncol(Surf):1] > 0
Surf[NAout] <- NA
breaks <- pretty(Surf,n=20)
pdf("/home/tim/git/APCTapps/APCTapps/Figures/meanBA1.pdf",width=6,height=6)
par(xaxs="i",yaxs="i",xpd=FALSE)
image(meanBA1$xat+.5,meanBA1$yat+.5,t(Surf),asp=1, 
		xlab = labels[meanBA1$x], ylab = labels[meanBA1$y],
		col=ramp(length(breaks)-1),breaks=breaks, 
		ylim= c(0,30), xlim=c(15,45), axes = FALSE,
		panel.first=list(
				polygon(x=c(15,45,15),y=c(0,0,30), border=NA,col=gray(.85)),
				ATLRefN(0:45,col1="white",col2="white",col3="white"),
				text(seq(15,45,5),0,seq(15,45,5),pos=1,xpd=TRUE),
				text(15,seq(0,30,5),seq(0,30,5),pos=2,xpd=TRUE)
		))
contour(meanBA1$xat,meanBA1$yat,t(Surf),add=TRUE)
ATLRefN(0:45,col1="#FFFFFF40",col2="#FFFFFF40",col3="#FFFFFF40")
dev.off()

meanBA2 <- SurfGeneric(Dat=Dat, triad.i = triads[[14]], 
		stat = "BA", agg = mean, labels = labels, 
		ramp=rev(ramp(20)),contour=TRUE,i=14,plot = FALSE,pars=TRUE)
# limit age retired to 35
Surf <- meanBA2$Surf
NAout <- row(Surf)-col(Surf)[,ncol(Surf):1] > -5
Surf[NAout] <- NA
breaks <- pretty(Surf,n=20)
pdf("/home/tim/git/APCTapps/APCTapps/Figures/meanBA2.pdf",width=6,height=6)
par(xaxs="i",yaxs="i",xpd=FALSE)
image(meanBA2$xat+.5,meanBA2$yat+.5,t(Surf),asp=1, 
		xlab = labels[meanBA2$x], ylab = labels[meanBA2$y],
		col=ramp(length(breaks)-1),breaks=breaks, 
		ylim= c(0,35), xlim=c(15,50), axes = FALSE,
		panel.first=list(
				polygon(x=c(15,50,15),y=c(0,0,35), border=NA,col=gray(.85)),
				ATLRefN(0:50,col1="white",col2="white",col3="white",xpd=TRUE),
				text(seq(15,50,5),0,seq(15,50,5),pos=1,xpd=TRUE),
				text(15,seq(0,35,5),seq(0,35,5),pos=2,xpd=TRUE)
		))
contour(meanBA2$xat+.5,meanBA2$yat+.5,t(Surf),add=TRUE)
ATLRefN(0:50,col1="#FFFFFF40",col2="#FFFFFF40",col3="#FFFFFF40")
dev.off()


meanBA3 <- SurfGeneric(Dat=Dat, triad.i = triads[[16]], 
		stat = "BA", agg = mean, labels = labels, 
		ramp=ramp(20),contour=TRUE,i=16,plot = FALSE,pars=TRUE)
# limit career length to 33 or 32
Surf <- meanBA3$Surf
NAout <- row(Surf)-col(Surf)[,ncol(Surf):1] > -3
Surf[NAout] <- NA
breaks <- pretty(Surf,n=20)
pdf("/home/tim/git/APCTapps/APCTapps/Figures/meanBA3.pdf",width=6,height=6)
par(xaxs="i",yaxs="i",xpd=FALSE)
image(meanBA3$xat+1.5,meanBA3$yat+.5,t(Surf),asp=1, 
		xlab = labels[meanBA3$x], ylab = labels[meanBA3$y],
		col=ramp(length(breaks)-1),breaks=breaks, 
		ylim= c(0,30), xlim=c(0,30), axes = FALSE,
		panel.first=list(
				polygon(x=c(0,30,0),y=c(0,0,30), border=NA,col=gray(.85)),
				ATLRefN(0:30,col1="white",col2="white",col3="white",xpd=TRUE),
				text(seq(0,30,5),0,seq(0,30,5),pos=1,xpd=TRUE),
				text(0,seq(0,30,5),seq(0,30,5),pos=2,xpd=TRUE)
		))
contour(meanBA3$xat+1.5,meanBA3$yat,t(Surf),add=TRUE)
ATLRefN(0:30,col1="#FFFFFF40",col2="#FFFFFF40",col3="#FFFFFF40")
dev.off()





labels[triads[[2]]]
# BA long surfs
CVBAp1 <- SurfGeneric(Dat=Dat, triad.i = triads[[2]], 
		stat = "BA", agg = cv, labels = labels, 
		ramp=rev(ramp(20)),contour=TRUE,i=2,plot = FALSE,pars=TRUE)
LexRefN(ages=15:50,years=1870:2015)
Surf <- CVBAp1$Surf
breaks <- pretty(Surf,n=20)
pdf("/home/tim/git/APCTapps/APCTapps/Figures/CVBAp1.pdf",width=19,height=6)
par(xaxs="i",yaxs="i",xpd=FALSE)
image(CVBAp1$xat+.5,CVBAp1$yat+.5,t(Surf),asp=1, 
		xlab = labels[CVBAp1$x], ylab = labels[CVBAp1$y],
		col=rev(ramp(length(breaks)-1)),breaks=breaks, 
		xlim= c(1870,2016), ylim=c(15,50), axes = FALSE,
		panel.first=list(
				rect(1870,15,2016,50, border=NA,col=gray(.85)),
				LexRefN(ages=15:50,years=1870:2015,col1="white",col2="white",col3="white",xpd=TRUE),
				text(seq(1870,2010,10),15,seq(1870,2010,10),pos=1,xpd=TRUE),
				text(1870,seq(15,50,5),seq(15,50,5),pos=2,xpd=TRUE)
		))
contour(CVBAp1$xat+.5,CVBAp1$yat+.5,t(Surf),add=TRUE)
ATLRefN(0:30,col1="#FFFFFF40",col2="#FFFFFF40",col3="#FFFFFF40")
LexRefN(ages=15:50,years=1870:2015,col1="#FFFFFF40",col2="#FFFFFF40",col3="#FFFFFF40",xpd=TRUE)
dev.off()


# y lim 15-50 APC
# OK
labels[triads[[3]]]
CVBAp2 <- SurfGeneric(Dat=Dat, triad.i = triads[[3]], 
		stat = "BA", agg = cv, labels = labels, 
		ramp=rev(ramp(20)),contour=TRUE,i=3,plot = TRUE,pars=TRUE)
Surf <- CVBAp2$Surf
a <- 16:50;y<- 1871:2015
Surf2 <- matrix(NA,nrow=length(a),ncol=length(y),dimnames=list(a,y))
ind <- as.integer(rownames(Surf)) < 51
Surf2[rownames(Surf[ind,]),colnames(Surf[ind,])] <- Surf[ind,]
Surf <- Surf2
CVBAp2$yat <- as.integer(rownames(Surf))
breaks <- pretty(Surf,n=20)
pdf("/home/tim/git/APCTapps/APCTapps/Figures/CVBAp2.pdf",width=19,height=6)
par(xaxs="i",yaxs="i",xpd=FALSE)
image(CVBAp2$xat+.5,CVBAp2$yat+.5,t(Surf),asp=1, 
		xlab = labels[CVBAp2$x], ylab = labels[CVBAp2$y],
		col=rev(ramp(length(breaks)-1)),breaks=breaks, 
		xlim= c(1870,2016), ylim=c(15,50), axes = FALSE,
		panel.first=list(
				rect(1870,15,2016,50, border=NA,col=gray(.85)),
				LexRefN(ages=15:50,years=1870:2015,col1="white",col2="white",col3="white",xpd=TRUE),
				text(seq(1870,2010,10),15,seq(1870,2010,10),pos=1,xpd=TRUE),
				text(1870,seq(15,50,5),seq(15,50,5),pos=2,xpd=TRUE)
		))
contour(CVBAp2$xat+.5,CVBAp2$yat+.5,t(Surf),add=TRUE)
LexRefN(ages=15:50,years=1870:2015,col1="#FFFFFF40",col2="#FFFFFF40",col3="#FFFFFF40",xpd=TRUE)
dev.off()
# y lim 15-50 (age retired in y). Maybe take birth coh after 1870?

# no need to cut these
labels[triads[[5]]]
CVBAp3 <- SurfGeneric(Dat=Dat, triad.i = triads[[5]], 
		stat = "TB", agg = cv, labels = labels, 
		ramp=rev(ramp(20)),contour=TRUE,i=5,pars=TRUE)
Surf <- CVBAp3$Surf
breaks <- pretty(Surf,n=20)
pdf("/home/tim/git/APCTapps/APCTapps/Figures/CVBAp3.pdf",width=19,height=6)
par(xaxs="i",yaxs="i",xpd=FALSE)
image(CVBAp3$xat+.5,CVBAp3$yat+1.5,t(Surf),asp=1, 
		xlab = labels[CVBAp3$x], ylab = labels[CVBAp3$y],
		col=rev(ramp(length(breaks)-1)),breaks=breaks, 
		xlim= c(1870,2016), ylim=c(0,30), axes = FALSE,
		panel.first=list(
				rect(1870,0,2016,30, border=NA,col=gray(.85)),
				LexRefN(ages=0:30,years=1870:2015,col1="white",col2="white",col3="white",xpd=TRUE),
				text(seq(1870,2010,10),0,seq(1870,2010,10),pos=1,xpd=TRUE),
				text(1870,seq(0,30,5),seq(0,30,5),pos=2,xpd=TRUE)
		))
contour(CVBAp3$xat+.5,CVBAp3$yat+1.5,t(Surf),add=TRUE)
LexRefN(ages=0:30,years=1870:2015,col1="#FFFFFF40",col2="#FFFFFF40",col3="#FFFFFF40",xpd=TRUE)
dev.off()




labels[triads[[8]]]
CVBAp4 <- SurfGeneric(Dat=Dat, 
		triad.i = triads[[8]], 
		stat = "TB", 
		agg = cv, 
		labels = labels, 
		ramp=rev(ramp(20)),
		contour=TRUE,i=8,pars=TRUE)
Surf <- CVBAp4$Surf
breaks <- pretty(Surf,n=20)
pdf("/home/tim/git/APCTapps/APCTapps/Figures/CVBAp4.pdf",width=19,height=6)
par(xaxs="i",yaxs="i",xpd=FALSE)
image(CVBAp4$xat+.5,CVBAp4$yat+1.5,t(Surf),asp=1, 
		xlab = labels[CVBAp4$x], ylab = labels[CVBAp4$y],
		col=rev(ramp(length(breaks)-1)),breaks=breaks, 
		xlim= c(1870,2016), ylim=c(0,30), axes = FALSE,
		panel.first=list(
				rect(1870,0,2016,30, border=NA,col=gray(.85)),
				LexRefN(ages=0:30,years=1870:2015,col1="white",col2="white",col3="white",xpd=TRUE,chrono=FALSE),
				text(seq(1870,2010,10),0,seq(1870,2010,10),pos=1,xpd=TRUE),
				text(1870,seq(0,30,5),seq(0,30,5),pos=2,xpd=TRUE)
		))
contour(CVBAp4$xat+.5,CVBAp4$yat+1.5,t(Surf),add=TRUE)
LexRefN(ages=0:30,years=1870:2015,col1="#FFFFFF40",col2="#FFFFFF40",col3="#FFFFFF40",xpd=TRUE,chrono=FALSE)
dev.off()


