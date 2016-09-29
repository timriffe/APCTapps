setwd("/home/tim/git/APCTapps/APCTapps")
# Author: tim
###############################################################################
# just draw up a prenatal TAL diagram
gridcol <- gray(.6) # medium gray, not overwhelming.
weeks   <- 0:45
wk5     <- seq(0, 45, by = 5)

pdf("Figures/PrenatalDiagram.pdf")
par(mai = c(1.5, .5, .5, .5), xpd = TRUE)
plot(NULL, 
		type = "n", 
		xlim = range(weeks), 
		ylim = range(weeks), 
		axes = FALSE, 
		xlab = "", 
		ylab = "", 
		asp = 1)
segments(rep(0, 10), wk5, wk5, rep(0, 10), col = gridcol)
#segments(111,0,111,1)
segments(0,0,45,0, col = gridcol)
segments(0,0,0,45, col = gridcol)
# horizontal:
segments(0,wk5, rev(wk5), wk5, col = gridcol)
# vertical
segments(wk5,0,wk5,rev(wk5), col = gridcol)

lrcol <- paste0(RColorBrewer::brewer.pal(9,"Set3")[2],"40")
# preterm
polygon(c(0,0,21,37),c(37,21,0,0),col="#CC790040",border=NA)
text(15,21,"Preterm",srt=-45)
# early term
polygon(c(0,0,37,39),c(39,37,0,0),col=lrcol,border=NA)
text(19,18.7,"Early term",srt=-45)
# full term
polygon(c(0,0,39,41),c(41,39,0,0),col="#1AFF0040",border=NA)
text(23,17,"Full term",srt=-45)
# late term
polygon(c(0,0,41,42),c(42,41,0,0),col=lrcol,border=NA)
text(27,15,"Late term",srt=-45)
# postterm
polygon(c(0,0,42,45),c(45,42,0,0),col="#CC790040",border=NA)
text(31,13,"Postterm",srt=-45)
# axes
text(wk5,-.5,wk5,pos=1)
text(-.5,wk5,wk5,pos=2)

# bold lines for conception

text(20,-5,"Weeks since conception")
text(-5,20,"Weeks until parturition",srt=90)
text(25,25,"Total gestation weeks",srt=-45)

dev.off()
