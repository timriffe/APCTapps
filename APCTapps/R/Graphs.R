
# Author: tim
###############################################################################


# Lahman data:
# CPD, debut, final

source("/home/tim/git/APCTapps/APCTapps/R/Functions.R")

nedges(5)
draw.timeline(p=seq(0,5,length=5),ylim=c(-5,0))
draw.tree(5,nedges(5), lprop=.4)
pcol  = "#e64dca"
opcol = "#6e4559"
odcol = "#c04a39"
dpcol = "#ee4120"
#edge.colors <- list(pcol = "#e64dca", opcol = "#6e4559", dpcol = "#c04a39", odcol = "#ee4120")

nedges(5)
col <- c(opcol,opcol,pcol, opcol,opcol, # event colors
		 #d1  d2    d3    d4    d5    d6     d7    d8     d9   d10
		odcol,dpcol,dpcol,odcol,odcol,dpcol,odcol,odcol,dpcol,odcol
		)
		
pdf("/home/tim/git/APCTapps/APCTapps/Figures/BaseballGraph.pdf",width=5,height=5)
par(mai=c(.3,.3,.3,.3),xaxs="i",yaxs="i")
draw.tree(5,nedges(5), lprop=.4,col = col, lwd=5,label=FALSE)
points(decide.verts(p=seq(0,5,length=6)), pch = 16, cex=3, col = c(opcol,opcol,pcol,opcol,opcol,opcol))
dev.off()

timeline.graph(5,label=FALSE,dcol=col[6:15],pcol=col[1:5],lwd=5)
draw.timeline(p=seq(0,5,length=5),ylim=c(-5,0),dcol=col[6:15],pcol=col[1:5],lwd=3)

# code used to generate baseball graph colors!
# this still won't recreate them, because it took several dice rolls each...
# -------------------------------

#devtools::install_github("hoesler/rwantshue")
#library(rwantshue)
#
## seed for reproducibility
#scheme <- iwanthue(seed = 42, force_init = TRUE) # recreate with a seed
#
## generate a new color palette (vector of hex values) with presets...
#plot(1:10,col=scheme$hex(10),pch=16,cex=10)
#
#scheme$hex(10, color_space = hcl_presets$fluo)

# ... or make custom adjustments:
# purples
#color_space <- list(
#		c(330, 360),	# hue range [0,360]
#		c(0, 100),		# chroma range [0,100]
#		c(20, 80))		# lightness range [0,100]
#
#
#purples <- scheme$hex(
#		n = 8,
#		force_mode = FALSE,
#		quality = 80,
#		color_space = color_space)

# use it in a plot
#plot(1:8, col = scheme$hex(8), pch = 16, cex = 10)
#plot(1:8, col = sort(purples), pch = 16, cex = 10)

# Period
#pcol <- purples[2]
## other p
#opcol <- purples[3]
#

# oranges
#color_space <- list(
#		c(20, 50),	# hue range [0,360]
#		c(0, 100),		# chroma range [0,100]
#		c(20, 80))		# lightness range [0,100]
#oranges <- scheme$hex(
#		n = 8,
#		force_mode = FALSE,
#		quality = 80,
#		color_space = color_space)

# use it in a plot
#plot(1:8, col = scheme$hex(8), pch = 16, cex = 10)
#plot(1:8, col = sort(oranges), pch = 16, cex = 10)

# durations touching Period
#dpcol <- oranges[8]
#odcol <- oranges[2]
