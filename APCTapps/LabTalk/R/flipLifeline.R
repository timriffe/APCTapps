setwd("/home/tim/git/APCTapps/APCTapps/LabTalk")
# Author: tim
###############################################################################
fore1 <- gray(.3)
fore2 <- gray(.1)
fore3 <- gray(.6)

pdf("Figures/boxflip1.pdf",width=10.2,height=1.2)
par(mai=c(.1,.1,.1,.1))
plot(NULL, type = "n", ylim = c(0,1), xlim = c(0,10), 
		xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "", asp = 1)
polygon(c(5,10,10),c(0,0,.8),border=NA,col=fore3)
rect(0,0,10,1,xpd=TRUE,border = fore2, lwd = 5,xpd=TRUE)
arrows(3.5,.5,6.5,.5,lwd=5,col=fore2)
dev.off()

pdf("Figures/boxflip2.pdf",width=10.2,height=1.2)
par(mai=c(.1,.1,.1,.1))
plot(NULL, type = "n", ylim = c(0,1), xlim = c(0,10), 
		xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "", asp = 1)
polygon(c(5,0,0),c(0,0,.8),border=NA,col=fore3)
rect(0,0,10,1,xpd=TRUE,border = fore2, lwd = 5,xpd=TRUE)
arrows(6.5,.5,3.5,.5,lwd=5,col=fore2)
dev.off()