
# Author: tim
###############################################################################
setwd("/home/tim/git/APCTapps/APCTapps/LabTalk")
# lifelines 1:
fore1 <- gray(.3)
fore2 <- gray(.1)


pdf("Figures/lifelines1.pdf",width=5.2,height=2.2)
par(mai=c(0,0,0,0))
plot(NULL, type = "n", ylim = c(-1,1), xlim = c(0,5), 
		xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "", asp = 1)
segments(c(.5,.5), c(.5,-.5), c(4.5,3.5), c(.5,-.5), lwd = 3,col=fore1)
points(c(.5,.5), c(.5,-.5), pch = 16, cex = 4,col=fore1)
points(c(4.5,3.5), c(.5,-.5), pch = 4, cex = 3, lwd =4,col=fore1)
dev.off()

pdf("Figures/lifelines2.pdf",width=5.2,height=2.2)
par(mai=c(0,0,0,0))
plot(NULL, type = "n", ylim = c(-1,1), xlim = c(0,5), 
		xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "", asp = 1)
segments(c(.5,.5), c(.5,-.5), c(4.5,3.5), c(.5,-.5), lwd = 3,col=fore1)
points(c(.5,.5), c(.5,-.5), pch = 16, cex = 4,col=fore1)
points(c(4.5,3.5), c(.5,-.5), pch = 4, cex = 3, lwd =4,col=fore1)
segments(1.5,c(.4,-.4), 1.5, c(.6,-.6), lwd = 4,col=fore2)
text(1.5, c(.6,-.4), "A",cex=1.6,pos=3,font=2,col=fore2)
segments(c(3.5,2.5),c(.4,-.4), c(3.5,2.5), c(.6,-.6), lwd = 4,col=fore2)
text(c(3.5,2.5), c(.6,-.4), "B",cex=1.6,pos=3,font=2,col=fore2)
dev.off()

pdf("Figures/lifelines3.pdf",width=5.2,height=2.2)
par(mai=c(0,0,0,0))
plot(NULL, type = "n", ylim = c(-1,1), xlim = c(0,5), 
		xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "", asp = 1)
segments(c(.5,1.5), c(.5,-.5), c(4.5,4.5), c(.5,-.5), lwd = 3,col=fore1)
points(c(.5,1.5), c(.5,-.5), pch = 16, cex = 4,col=fore1)
points(c(4.5,4.5), c(.5,-.5), pch = 4, cex = 3, lwd =4,col=fore1)
segments(c(1.5,2.5),c(.4,-.4), c(1.5,2.5), c(.6,-.6), lwd = 4,col=fore2)
text(c(1.5,2.5), c(.6,-.4), "A",cex=1.6,pos=3,font=2,col=fore2)
segments(c(3.5,3.5),c(.4,-.4), c(3.5,3.5), c(.6,-.6), lwd = 4,col=fore2)
text(c(3.5,3.5), c(.6,-.4), "B",cex=1.6,pos=3,font=2,col=fore2)
dev.off()


