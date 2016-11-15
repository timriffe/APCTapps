#Author: tim
###############################################################################
setwd("/home/tim/git/APCTapps/APCTapps/LabTalk")
# source("R/SurvivalJapan.R")
####################################################################################
# Digaram 2
library(HMDHFDplus)
#LT <- readHMDweb("JPN","mltper_1x1",username=us,password=pw)
#lx <- LT$lx[LT$Year == 2010] / 1e5
fore1 <- gray(.3)
fore2 <- gray(.1)
fore3 <- gray(.6)
tcol  <- gray(.4) #"#666666"  "#666666"
ccol  <- gray(.1) # "#1A1A1A" "#1A1A1A"
#1A1A1ACC


drawRect <- function(y1,y2,L,ThanoStart = 5,ThanoMax = .8,ChronoStart = 45,ChronoOmega=110,ChronoMax = .8){
	
	# lifespan rect
	rect(0,y2,L,y1,border="white",col=gray(.9))
	
	# chronological pattern:
	if (ChronoStart < L){
		ydelta 		<- (y2 - y1) * ChronoMax
		slope 		<- ydelta / (ChronoOmega - ChronoStart)
		Lintercept 	<- y1 + slope * (L - ChronoStart)
		polygon(x=c(ChronoStart,L,L),y=c(y1,y1,Lintercept), border = "white", col = "#1A1A1ACC")
		area <- abs(ChronoStart - L) * (Lintercept - y1) * 5
	} else {
		area <- 0
	}
	
	
	# thanatological pattern:
	Tdelta <- (y2 - y1) * ThanoMax
	Tintercept <- y1 + Tdelta
	Tslope <- Tdelta / L
	if (L < ThanoStart){
		TinterceptLeft <- y1 + (ThanoStart - L) * Tslope
		xleft <- 0
		polygon(x=c(0,0,L,L),y=c(TinterceptLeft,y1,y1,Tintercept), border = "white", col = "#66666670")
	} else {
		polygon(x=c(L-ThanoStart,L,L),y=c(y1,y1,Tintercept), border = "white", col = "#66666670")
	}
	invisible(area)
}





#plot(NULL, type = "n", xlim = c(0,110), ylim = c(0,1))
#for (i in 1:(length(a)-1)){
#	drawRect(q[i+1],q[i],a[i])
#}


drawComparison <- function(lx,
		q=seq(0,1,by=.1),
		round=TRUE,
		ThanoStart = 5,
		ThanoMax = .8,
		ChronoStart = 45,
		ChronoOmega=110,
		ChronoMax = .8,...){
	
	A <- splinefun(0:110~lx)(q)
	
	AA <- c()
	for (i in 1:10){
		xx    <- seq(A[i],A[i+1],by=.01)
		lxx   <- splinefun(lx*0:110)(xx)
		AA[i] <- sum(lxx*xx)/sum(lxx)
	}
	
	if (round){
		a <- round(AA)
	} else {
		a <- AA
	}
	
	plot(NULL, type = "n", xlim = c(0,110), ylim = c(0,1), xlab = "Age", ylab = "l(x)",...)
	lines(0:110,lx)
	for (i in 1:length(a)){
		drawRect(q[i+1],q[i],a[i],
				ThanoStart = 5,
				ThanoMax = .8,
				ChronoStart = 45,
				ChronoOmega=110,
				ChronoMax = .8)
	}
}


LT    <- readHMDweb("JPN","mltper_1x1",username=us,password=pw)
years <- sort(unique(LT$Year))

#library(animation)
#getwd()
#list.files()
#saveGIF({for (y in years){
#			lx <- LT$lx[LT$Year == y] / 1e5
#			drawComparison(lx,seq(1,0,by=-.1),
#					round=FALSE,main = y)
#			#Sys.sleep(1)
#		}}, movie.name = "JapanHealthDemo.gif", interval = 0.1, nmax = 50, ani.width = 600, 
#		ani.height = 600)

##########################
# make prettier for the manuscript:
LT$ex[LT$Year == 1970]
LT$ex[LT$Year == 2010]
# 1970 Figure
q           <- seq(1,0,by=-.1)
ThanoMax    <- .8
ThanoStart  <- 6
ChronoOmega <- 111.5
lx          <- LT$lx[LT$Year == 1970] / 1e5
A           <- splinefun(0:110~lx)(q)
a           <- c()
for (i in 1:10){
	xx    <- seq(A[i],A[i+1],by=.01)
	lxx   <- splinefun(lx*0:110)(xx)
	a[i]  <- sum(lxx*xx)/sum(lxx)
}
graphics.off()
#dev.new(width=8,height=5)

pdf("Figures/Japan1.pdf",width=7,height=5)
par(mai=c(.5,.4,.1,0),xpd=TRUE)
plot(NULL, type = "n", xlim = c(0, 140), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE,asp=100)
lines(0:110, lx, col = gray(.2))

segments(0,0,0,1,lwd=2,xpd=TRUE,col=fore2)
segments(0,0,110,0,lwd=2,xpd=TRUE,col=fore2)
# x axis
segments(seq(0,100,by=20),0,seq(0,100,by=20),-.01,xpd=TRUE,col=fore2)
text(seq(0,100,by=20),-.01,seq(0,100,by=20),pos=1,xpd=TRUE,col=fore2)
# y axis
segments(0, seq(0, 1, by = .2), -1, seq(0, 1, by = .2), xpd = TRUE,col=fore2)
text(-1, seq(0, 1, by = .2), seq(0, 1, by = .2),pos = 2, xpd = TRUE,col=fore2)
# labels
text(55, -.1, "Age", xpd = TRUE, cex = 1.2)
text(-13, .5, "l(x)", xpd = TRUE, cex = 1.2)
dev.off()

# add bars for discretization
pdf("Figures/Japan2.pdf",width=7,height=5)
par(mai=c(.5,.4,.1,0),xpd=TRUE)
plot(NULL, type = "n", xlim = c(0, 140), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE,asp=100)
lines(0:110, lx, col = gray(.2))
chronoA <- c()
for (i in 1:length(a)){
	chronoA[i] <- drawRect(q[i + 1], q[i], a[i],
			ThanoStart = ThanoStart,
			ThanoMax = 0,
			ChronoStart = 0,
			ChronoOmega = ChronoOmega,
			ChronoMax = 0)
}
for (v in seq(0,100,by=20)){
	ymax <- q[a > v][1]
	segments(v,0,v,ymax,col="white")
}

segments(0,0,0,1,lwd=2,xpd=TRUE,col=fore2)
segments(0,0,110,0,lwd=2,xpd=TRUE,col=fore2)
# x axis
segments(seq(0,100,by=20),0,seq(0,100,by=20),-.01,xpd=TRUE,col=fore2)
text(seq(0,100,by=20),-.01,seq(0,100,by=20),pos=1,xpd=TRUE,col=fore2)
# y axis
segments(0, seq(0, 1, by = .2), -1, seq(0, 1, by = .2), xpd = TRUE,col=fore2)
text(-1, seq(0, 1, by = .2), seq(0, 1, by = .2),pos = 2, xpd = TRUE,col=fore2)
# labels
text(55, -.1, "Age", xpd = TRUE, cex = 1.2)
text(-13, .5, "l(x)", xpd = TRUE, cex = 1.2)
dev.off()


# add bars for discretization
pdf("Figures/Japan3.pdf",width=7,height=5)
par(mai=c(.5,.4,.1,0),xpd=TRUE)
plot(NULL, type = "n", xlim = c(0, 140), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE,asp=100)
#lines(0:110, lx, col = gray(.2))
chronoA <- c()
for (i in 1:length(a)){
	chronoA[i] <- drawRect(q[i + 1], q[i], a[i],
			ThanoStart = ThanoStart,
			ThanoMax = ThanoMax,
			ChronoStart = 50,
			ChronoOmega = ChronoOmega,
			ChronoMax = 0)
}
## bar blocks this can be done just over bars:
for (v in seq(0,100,by=20)){
	ymax <- q[a > v][1]
	segments(v,0,v,ymax,col="white")
}

segments(0,0,0,1,lwd=2,xpd=TRUE,col=fore2)
segments(0,0,110,0,lwd=2,xpd=TRUE,col=fore2)
# x axis
segments(seq(0,100,by=20),0,seq(0,100,by=20),-.01,xpd=TRUE,col=fore2)
text(seq(0,100,by=20),-.01,seq(0,100,by=20),pos=1,xpd=TRUE,col=fore2)
# y axis
segments(0, seq(0, 1, by = .2), -1, seq(0, 1, by = .2), xpd = TRUE,col=fore2)
text(-1, seq(0, 1, by = .2), seq(0, 1, by = .2),pos = 2, xpd = TRUE,col=fore2)
# labels
text(55, -.1, "Age", xpd = TRUE, cex = 1.2)
text(-13, .5, "l(x)", xpd = TRUE, cex = 1.2)
# label triangles:
segments(a[1]-1, mean(c(q[2], q[1:2])), a[1] + 15,q[1], col = gray(.5), lty = 3)
text(a[1] + 15, q[1], "TTD process", pos = 4, cex = 1.2)
#segments(82, .02, 90, .18, col = gray(.5), lty = 3)
#text(90, .18, "Age process", pos = 4, cex = 1.2)

#segments(110,.05,110,.95,col=gray(.8))
## summary squares
ThanoTotal  <- (ThanoStart * ThanoMax) / 2 * 10
ChronoTotal <- sum(chronoA)
#
width <- 10
rect(120,0,130,((ThanoTotal / width) / 10),col = "#66666670", border = "white")
#rect(131,0,141,((ChronoTotal / width) / 10),col = "#1A1A1ACC", border = "white")
text(125,-.05,"TTD",srt=90)
#text(136,-.05,"Age",srt=90)
segments(120,seq(.1,.2,by=.1),141,seq(.1,.2,by=.1),col="white")
text(142,seq(0,.2,by=.1),0:2,pos=4)
text(130,-.12,"total DLY")
dev.off()



# add bars for discretization
pdf("Figures/Japan4.pdf",width=7,height=5)
par(mai=c(.5,.4,.1,0),xpd=TRUE)
plot(NULL, type = "n", xlim = c(0, 140), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE,asp=100)
#lines(0:110, lx, col = gray(.2))
chronoA <- c()
for (i in 1:length(a)){
	chronoA[i] <- drawRect(q[i + 1], q[i], a[i],
			ThanoStart = ThanoStart,
			ThanoMax = 0,
			ChronoStart = 50,
			ChronoOmega = ChronoOmega,
			ChronoMax = .5)
}
## bar blocks this can be done just over bars:
for (v in seq(0,100,by=20)){
	ymax <- q[a > v][1]
	segments(v,0,v,ymax,col="white")
}

segments(0,0,0,1,lwd=2,xpd=TRUE,col=fore2)
segments(0,0,110,0,lwd=2,xpd=TRUE,col=fore2)
# x axis
segments(seq(0,100,by=20),0,seq(0,100,by=20),-.01,xpd=TRUE,col=fore2)
text(seq(0,100,by=20),-.01,seq(0,100,by=20),pos=1,xpd=TRUE,col=fore2)
# y axis
segments(0, seq(0, 1, by = .2), -1, seq(0, 1, by = .2), xpd = TRUE,col=fore2)
text(-1, seq(0, 1, by = .2), seq(0, 1, by = .2),pos = 2, xpd = TRUE,col=fore2)
# labels
text(55, -.1, "Age", xpd = TRUE, cex = 1.2)
text(-13, .5, "l(x)", xpd = TRUE, cex = 1.2)
# label triangles:
#segments(a[1]-1, mean(c(q[2], q[1:2])), a[1] + 15,q[1], col = gray(.5), lty = 3)
#text(a[1] + 15, q[1], "TTD process", pos = 4, cex = 1.2)
segments(82, .02, 90, .18, col = gray(.5), lty = 3)
text(90, .18, "Age process", pos = 4, cex = 1.2)

#segments(110,.05,110,.95,col=gray(.8))
## summary squares
ThanoTotal  <- (ThanoStart * ThanoMax) / 2 * 10
ChronoTotal <- sum(chronoA)
#
width <- 10
rect(120,0,130,((ThanoTotal / width) / 10),col = "#66666670", border = "white")
rect(131,0,141,((ChronoTotal / width) / 10),col = "#1A1A1ACC", border = "white")
text(125,-.05,"TTD",srt=90)
text(136,-.05,"Age",srt=90)
segments(120,seq(.1,.2,by=.1),141,seq(.1,.2,by=.1),col="white")
text(142,seq(0,.2,by=.1),0:2,pos=4)
text(130,-.12,"total DLY")
dev.off()


# add bars for discretization
pdf("Figures/Japan5.pdf",width=7,height=5)
par(mai=c(.5,.4,.1,0),xpd=TRUE)
plot(NULL, type = "n", xlim = c(0, 140), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE,asp=100)
#lines(0:110, lx, col = gray(.2))
chronoA <- c()
for (i in 1:length(a)){
	chronoA[i] <- drawRect(q[i + 1], q[i], a[i],
			ThanoStart = ThanoStart,
			ThanoMax = ThanoMax,
			ChronoStart = 50,
			ChronoOmega = ChronoOmega,
			ChronoMax = .5)
}
## bar blocks this can be done just over bars:
for (v in seq(0,100,by=20)){
	ymax <- q[a > v][1]
	segments(v,0,v,ymax,col="white")
}

segments(0,0,0,1,lwd=2,xpd=TRUE,col=fore2)
segments(0,0,110,0,lwd=2,xpd=TRUE,col=fore2)
# x axis
segments(seq(0,100,by=20),0,seq(0,100,by=20),-.01,xpd=TRUE,col=fore2)
text(seq(0,100,by=20),-.01,seq(0,100,by=20),pos=1,xpd=TRUE,col=fore2)
# y axis
segments(0, seq(0, 1, by = .2), -1, seq(0, 1, by = .2), xpd = TRUE,col=fore2)
text(-1, seq(0, 1, by = .2), seq(0, 1, by = .2),pos = 2, xpd = TRUE,col=fore2)
# labels
text(55, -.1, "Age", xpd = TRUE, cex = 1.2)
text(-13, .5, "l(x)", xpd = TRUE, cex = 1.2)
# label triangles:
#segments(a[1]-1, mean(c(q[2], q[1:2])), a[1] + 15,q[1], col = gray(.5), lty = 3)
#text(a[1] + 15, q[1], "TTD process", pos = 4, cex = 1.2)
#segments(82, .02, 90, .18, col = gray(.5), lty = 3)
#text(90, .18, "Age process", pos = 4, cex = 1.2)

#segments(110,.05,110,.95,col=gray(.8))
## summary squares
ThanoTotal  <- (ThanoStart * ThanoMax) / 2 * 10
ChronoTotal <- sum(chronoA)
#
width <- 10
rect(120,0,130,((ThanoTotal / width) / 10),col = "#66666670", border = "white")
rect(131,0,141,((ChronoTotal / width) / 10),col = "#1A1A1ACC", border = "white")
text(125,-.05,"TTD",srt=90)
text(136,-.05,"Age",srt=90)
segments(120,seq(.1,.2,by=.1),141,seq(.1,.2,by=.1),col="white")
text(142,seq(0,.2,by=.1),0:2,pos=4)
text(130,-.12,"total DLY")
dev.off()



# 2010 Figure
ThanoMax   <- .8
ThanoStart <- 6
ChronoOmega <- 111.5
lx <- LT$lx[LT$Year == 2010] / 1e5
A  <- splinefun(0:110~lx)(q)
a <- c()
for (i in 1:10){
	xx    <- seq(A[i],A[i+1],by=.01)
	lxx   <- splinefun(lx*0:110)(xx)
	a[i]  <- sum(lxx*xx)/sum(lxx)
}
graphics.off()

#dev.new(width=8,height=5)
pdf("Figures/Japan6.pdf",width=7,height=5)
par(mai=c(.5,.4,.1,0),xpd=TRUE)
plot(NULL, type = "n", xlim = c(0, 140), ylim = c(0, 1), xlab = "", ylab = "", axes = FALSE,asp=100)
#lines(0:110, lx, col = gray(.2))
chronoA <- c()
for (i in 1:length(a)){
	chronoA[i] <- drawRect(q[i + 1], q[i], a[i],
			ThanoStart = ThanoStart,
			ThanoMax = ThanoMax,
			ChronoStart = 50,
			ChronoOmega = ChronoOmega,
			ChronoMax = .5)
}
# bar blocks this can be done just over bars:
for (v in seq(0,100,by=20)){
	ymax <- q[a > v][1]
	segments(v,0,v,ymax,col="white")
}
segments(0,0,0,1,lwd=2,xpd=TRUE,col=fore2)
segments(0,0,110,0,lwd=2,xpd=TRUE,col=fore2)
# x axis
segments(seq(0,100,by=20),0,seq(0,100,by=20),-.01,xpd=TRUE)
text(seq(0,100,by=20),-.01,seq(0,100,by=20),pos=1,xpd=TRUE)
# y axis
segments(0, seq(0, 1, by = .2), -1, seq(0, 1, by = .2), xpd = TRUE)
text(-1, seq(0, 1, by = .2), seq(0, 1, by = .2),pos = 2, xpd = TRUE)
# labels
text(55, -.1, "Age", xpd = TRUE, cex = 1.2)
text(-13, .5, "l(x)", xpd = TRUE, cex = 1.2)

# summary squares
ThanoTotal  <- (ThanoStart * ThanoMax) / 2 * 10
ChronoTotal <- sum(chronoA)

width <- 10
rect(120,0,130,((ThanoTotal / width) / 10),col = "#66666670", border = "white")
rect(131,0,141,((ChronoTotal / width) / 10),col = "#1A1A1ACC", border = "white")
text(125,-.05,"TTD",srt=90)
text(136,-.05,"Age",srt=90)
segments(120,seq(.1,.4,by=.1),141,seq(.1,.4,by=.1),col="white")
text(142,seq(0,.4,by=.1),0:4,pos=4)
text(130,-.12,"total DLY")
dev.off()# 
# 
# Author: tim
###############################################################################



