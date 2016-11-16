#
# Author: tim
###############################################################################
setwd("/home/tim/git/APCTapps/APCTapps/LabTalk")
grid.res <- 10
Nyears   <- 10
n        <- grid.res * Nyears


# ad hic function to make pi (for a given lifespan)
piexp2 <- function(grid.res = 10, Nyears = 10, lamda1 = .6, lambda2 = .27){
	n        <- grid.res * Nyears
	round((dexp(0:(n - 1), lamda1) + dexp(seq(0,grid.res, length = n),lambda2)) * grid.res)
}

# make L (gridded lifespan) given pi
pi2L <- function(pi, grid.res = 10){
	g <- 1:grid.res - 1
	outer(g, pi, "<")
}

# show L, stacked box grid.
plot.L <- function(L, add = FALSE, col = c("black","white"), border = "white", bg = gray(.9), xshift=0,yshift=0,...){
	grid.res <- nrow(L)
	x   <- (col(L) - 1) / grid.res
	y   <- (row(L) - 1) / grid.res
	# assuming regular L
	xat <- 1:(ncol(L) / grid.res)
	if (!add){
		plot(NULL, type = "n", xlim = c(0,max(xat)), ylim = c(0,1), 
				xaxs = "i", yaxs = "i",ylab = "pi",xlab = "TTD", asp = 1, ...)
	}
	rect(0+xshift,0+yshift,max(xat)+xshift,1+yshift,col=bg,border = NA)
	rect(x+xshift,y+yshift,x+1/grid.res+xshift,y+1/grid.res+yshift,
			col=ifelse(L,col[1],NA),
			border=ifelse(L,col[2],NA),
			lwd=.5)
	segments(xat+xshift,0+yshift,xat+xshift,1+yshift,col = col[2])
}

# this is unhealthy life expectancy
epi <- function(L){
	Nyears <- ncol(L) / nrow(L)
	(sum(L) / length(L)) * Nyears
}

# what proportion of unhealthy life lived 
# is in years < unhealthy life expectancy, 
# i.e., a measure of concentration
cpi <- function(L){
	Nyears   <- ncol(L) / nrow(L)
	n        <- ncol(L)
	grid.res <- nrow(L)
	Epi      <- epi(L)
	EpiN     <- Epi * grid.res
	# but if EpiN is a decimal, we do this in 3 steps.
	# these are index positions, so as not to get confused
	EpiNl    <- floor(EpiN) 
	EpiNm    <- floor(EpiN) + 1
	EpiNr    <- floor(EpiN) + 2
	
	# pi on the left
	LeftSum  <- ifelse(EpiNl > 0, sum(L[,1:EpiNl]),0)
	# pi on the right
	#RightSum <- ifelse(EpiNr <= n,sum(L[,EpiNr:n]),0)
    # pi in the middle
	fracr    <- EpiNm - EpiN
	fracl    <- 1 - fracr
	mSum     <- sum(L[, EpiNm])
	LeftSum  <- LeftSum + mSum * fracl
	#RightSum <- RightSum + mSum * fracr
	
	LeftSum / sum(L)
}

# avg squared distance of 
vpi <- function(L){
	Nyears    <- ncol(L) / nrow(L)
	n         <- ncol(L)
	grid.res  <- nrow(L)
	L[L == 0] <- NA
	distances <- (col(L) - .5) / grid.res
	mean(L * distances ^ 2, na.rm = TRUE)
}
sdpi <- function(L){
	sqrt(vpi(L))
}
medL <- function(L){
	dims <- dim(L)
	L <- as.logical(L)
	dim(L) <- dims
	n         <- dims[2]
	grid.res  <- nrow(L)
	Nyears    <- n / grid.res
	medi      <- round(sum(L) / 2)
	# column index
	coli      <- col(L)[which(L)[medi]]
	(coli + .5) / grid.res
}

api <- function(L){
	Nyears    <- ncol(L) / nrow(L)
	n         <- ncol(L)
	grid.res  <- nrow(L)
	L[L == 0] <- NA
	distances <- (col(L) - .5) / grid.res
	mean(L * distances, na.rm = TRUE)
}
sqapi <- function(L){
	Nyears    <- ncol(L) / nrow(L)
	n         <- ncol(L)
	grid.res  <- nrow(L)
	L[L == 0] <- NA
	distances <- (col(L) - .5) / grid.res
	mean(L * sqrt(distances), na.rm = TRUE)
}


pi <- piexp2(10, 10, .6, .27)
L <- pi2L(pi,10)
epi(L)
medL(L)
cpi(L)
vpi(L)
sdpi(L)
sdpi(pi2L(piexp2(10, 10, .8, .05),10))
plot.L(L, col = c("black","white"), axes = FALSE)
text(0:10,0,0:10,pos=1,xpd=TRUE)
text(0,0:1,0:1,pos=2,xpd=TRUE)
abline(v=epi(L),col="blue")
abline(v=medL(L),col="red")


#L2 <- L
#L2 <- L2 * 0
#L2[1, ] <- TRUE
#
#plot.L(L2, col = c("black","white"), axes = FALSE)
#text(0:10,0,0:10,pos=1,xpd=TRUE)
#text(0,0:1,0:1,pos=2,xpd=TRUE)

# dang lost ample matrices:



L0 <- L * 0
L0[,1:10] <- TRUE

L1 <- L * 0
L1[,1:6] <- TRUE
L1[1:9,7] <- TRUE
L1[1:7,8] <-TRUE
L1[1:6,9] <-TRUE
L1[1:4,10] <-TRUE
L1[1:3,11:12] <-TRUE
L1[1:2,13:14] <-TRUE
L1[1,1:18] <-TRUE
plot.L(L1, col = c("black","white"), axes = FALSE)
Lfries <- pi2L(c(10,rep(9:1,each=2),rep(0,81)),10)
plot.L(Lfries, col = c("black","white"), axes = FALSE)

L2 <- L * 0
L2[,1]      <- TRUE
L2[1:7,2]   <- TRUE
L2[5,1:3]   <- TRUE
L2[4,1:5]   <- TRUE
L2[3,1:6]   <- TRUE
L2[2,1:22]  <- TRUE
L2[1,1:57] <- TRUE
sum(L2)
plot.L(L2, col = c("black","white"), axes = FALSE)

L3 <- L * 0
L3[5,1] <- TRUE
L3[4,1:2] <- TRUE
L3[3, 1:3] <- TRUE
L3[2,1:5] <- TRUE
L3[1,1:89] <- TRUE
sum(L3)
plot.L(L3, col = c("black","white"), axes = FALSE)

L4 <- L * 0
L4[1, ] <- TRUE


L5 <- L0 * 0
L5[,1] <- TRUE

epi(L0);epi(L1);epi(Lfries);epi(L2);epi(L3);epi(L4);epi(L5)
medL(L0);medL(L1);medL(Lfries);medL(L2);medL(L3);medL(L4);medL(L5)
cpi(L0);cpi(L1);cpi(Lfries);cpi(L2);cpi(L3);cpi(L4);cpi(L5)
vpi(L0);vpi(L1);vpi(Lfries);vpi(L2);vpi(L3);vpi(L4);vpi(L5)
sdpi(L0);sdpi(L1);sdpi(Lfries);sdpi(L2);sdpi(L3);sdpi(L4);sdpi(L5)
api(L0);api(L1);api(Lfries);api(L2);api(L3);api(L4);api(L5)
sqapi(L0);sqapi(L1);sqapi(Lfries);sqapi(L2);sqapi(L3);sqapi(L4);sqapi(L5)


# -----------------
pdf("Figures/CompareTTD1.pdf",width=6,height=6)
xat <- 1:(ncol(L) / grid.res)
plot(NULL, type = "n", xlim = c(0,max(xat)), ylim = c(0,8.5), 
		xaxs = "i", yaxs = "i",ylab = "prevalence",xlab = "time-to-death", asp = 1, axes=FALSE)
plot.L(L0, col = c("black","white"), add=TRUE, yshift=7.5)
plot.L(L1, col = c("black","white"), add=TRUE, yshift=4.5)
plot.L(Lfries, col = c("black","white"), add=TRUE, yshift=6)
plot.L(L2, col = c("black","white"), add=TRUE, yshift=3)
plot.L(L3, col = c("black","white"), add=TRUE, yshift=1.5)
plot.L(L4, col = c("black","white"), add=TRUE, yshift=0)
text(0:10,0,0:10,pos=1,xpd=TRUE)
text(0,0:1,0:1,pos=2,xpd=TRUE)
dev.off()


pdf("Figures/CompareTTD2.pdf",width=6,height=6)
xat <- 1:(ncol(L) / grid.res)
plot(NULL, type = "n", xlim = c(0,max(xat)), ylim = c(0,8.5), 
	xaxs = "i", yaxs = "i",ylab = "prevalence",xlab = "time-to-death", asp = 1, axes=FALSE)
plot.L(L0, col = c("black","white"), add=TRUE, yshift=7.5)
segments(api(L0),7.4,api(L0),8.6,col = "red",lwd=4)
plot.L(L1, col = c("black","white"), add=TRUE, yshift=6)
segments(api(L1),5.9,api(L1),7.1,col = "red",lwd=4)
plot.L(Lfries, col = c("black","white"), add=TRUE, yshift=4.5)
segments(api(Lfries),4.4,api(Lfries),5.6,col = "red",lwd=4)
plot.L(L2, col = c("black","white"), add=TRUE, yshift=3)
segments(api(L2),2.9,api(L2),4.1,col = "red",lwd=4)
plot.L(L3, col = c("black","white"), add=TRUE, yshift=1.5)
segments(api(L3),1.4,api(L3),2.6,col = "red",lwd=4)
plot.L(L4, col = c("black","white"), add=TRUE, yshift=0)
segments(api(L4),-.1,api(L4),1.1,col = "red",lwd=4)
text(0:10,0,0:10,pos=1,xpd=TRUE)
text(0,0:1,0:1,pos=2,xpd=TRUE)
dev.off()

pdf("Figures/CompareTTD3.pdf",width=6,height=6)
xat <- 1:(ncol(L) / grid.res)
plot(NULL, type = "n", xlim = c(0,max(xat)), ylim = c(0,8.5), 
		xaxs = "i", yaxs = "i",ylab = "prevalence",xlab = "time-to-death", asp = 1, axes=FALSE)
plot.L(L0, col = c("black","white"), add=TRUE, yshift=7.5)
segments(sdpi(L0),7.4,sdpi(L0),8.6,col = "red",lwd=4)
plot.L(L1, col = c("black","white"), add=TRUE, yshift=6)
segments(sdpi(L1),5.9,sdpi(L1),7.1,col = "red",lwd=4)
plot.L(Lfries, col = c("black","white"), add=TRUE, yshift=4.5)
segments(sdpi(Lfries),4.4,sdpi(Lfries),5.6,col = "red",lwd=4)
plot.L(L2, col = c("black","white"), add=TRUE, yshift=3)
segments(sdpi(L2),2.9,sdpi(L2),4.1,col = "red",lwd=4)
plot.L(L3, col = c("black","white"), add=TRUE, yshift=1.5)
segments(sdpi(L3),1.4,sdpi(L3),2.6,col = "red",lwd=4)
plot.L(L4, col = c("black","white"), add=TRUE, yshift=0)
segments(sdpi(L4),-.1,sdpi(L4),1.1,col = "red",lwd=4)
text(0:10,0,0:10,pos=1,xpd=TRUE)
text(0,0:1,0:1,pos=2,xpd=TRUE)
dev.off()

pdf("Figures/CompareTTD4.pdf",width=6,height=6)
xat <- 1:(ncol(L) / grid.res)
plot(NULL, type = "n", xlim = c(0,max(xat)), ylim = c(0,8.5), 
		xaxs = "i", yaxs = "i",ylab = "prevalence",xlab = "time-to-death", asp = 1, axes=FALSE)
plot.L(L0, col = c("black","white"), add=TRUE, yshift=7.5)
segments(sqapi(L0),7.4,sqapi(L0),8.6,col = "red",lwd=4)
plot.L(L1, col = c("black","white"), add=TRUE, yshift=6)
segments(sqapi(L1),5.9,sqapi(L1),7.1,col = "red",lwd=4)
plot.L(Lfries, col = c("black","white"), add=TRUE, yshift=4.5)
segments(sqapi(Lfries),4.4,sqapi(Lfries),5.6,col = "red",lwd=4)
plot.L(L2, col = c("black","white"), add=TRUE, yshift=3)
segments(sqapi(L2),2.9,sqapi(L2),4.1,col = "red",lwd=4)
plot.L(L3, col = c("black","white"), add=TRUE, yshift=1.5)
segments(sqapi(L3),1.4,sqapi(L3),2.6,col = "red",lwd=4)
plot.L(L4, col = c("black","white"), add=TRUE, yshift=0)
segments(sqapi(L4),-.1,sqapi(L4),1.1,col = "red",lwd=4)
text(0:10,0,0:10,pos=1,xpd=TRUE)
text(0,0:1,0:1,pos=2,xpd=TRUE)
dev.off()
# -----------------

plot.L(Lfries, col = c("black","white"))

#
#time <- c(19.09, 19.55, 17.89, 17.73, 25.15, 27.27, 25.24, 21.05, 21.65, 20.92, 22.61, 15.71, 22.04, 22.60, 24.25)
#hist(time)
##install.packages("moments")
#library(moments)
#skewness(time)
#kurtosis(time)

# and trying with symmetry (fake half)

vpi2 <- function(L){
	Nyears    <- ncol(L) / nrow(L)
	n         <- ncol(L)
	grid.res  <- nrow(L)
	L[L == 0] <- NA
	distances <- (col(L) - .5) / Nyears
	distances <- cbind(-distances[,n:1], distances)
	L         <- cbind(L[, n:1], L) 
	mean(L * distances ^ 2, na.rm = TRUE)
}
vpi2(L) ; vpi(L) 

skewness(c(L) * c(distances))
kurtosis(c(L) * c(distances), na.rm=TRUE)
?kurtosis
kurtosis(rnorm(1000))

pi / 10
#install.packages("ineq")
library(ineq)
ineq(L * distances, type = "Gini")
plot(Lc(L*distances))
ineq(pi)
plot(pi/10)
pi <- pi/10
1/2 * sum(pi/sum(pi) - (1-pi)/sum(1-pi))
sum(pi^2)/sum(pi)

## from whuber: http://stats.stackexchange.com/questions/122668/is-there-a-measure-of-evenness-of-spread
#Ripley.K <- function(x, scale) {
#	# Arguments:
#	# x is an array of data.
#	# scale (not actually used) is an option to rescale the data.
#	#
#	# Return value:
#	# A function that calculates Ripley's K for any value between 0 and 1 (or `scale`).
#	#
#	x.pairs <- outer(x, x, function(a,b) abs(a-b))  # All pairwise distances
#	x.pairs <- x.pairs[lower.tri(x.pairs)]          # Distances between distinct pairs
#	if(missing(scale)) scale <- diff(range(x.pairs))# Rescale distances to [0,1]
#	x.pairs <- x.pairs / scale
#	#
#	# The built-in `ecdf` function returns the proportion of values in `x.pairs` that
#	# are less than or equal to its argument.
#	#
#	return (ecdf(x.pairs))
#}
##
## The one-dimensional L function.
## It merely subtracts 1 - (1-y)^2 from `Ripley.K(x)(y)`.  
## Its argument `x` is an array of data values.
##
#Ripley.L <- function(x) {function(y) Ripley.K(x)(y) - 1 + (1-y)^2}


eat <- c(distances*L)
plot(Ripley.L(eat[!is.na(eat)])(1:10))



piunif <- rep(sum(pi) / length(pi),length(pi))
sum(abs(cumsum(piunif) - cumsum(pi)))


pi     <- piexp2(10, 10, .6, .27) / 100
piunif <- rep(sum(pi) / length(pi),length(pi))
sum(abs(cumsum(piunif) - cumsum(pi)))

plot(cumsum(pi) / sum(pi))



