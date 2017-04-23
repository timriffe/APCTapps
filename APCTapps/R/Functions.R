# Author: tim
###############################################################################
library(expm)
# utility functions snagged from YearsLost repo.
degrees2radians <- function(degrees){
	degrees * (pi / 180)
}

quarterArc <- function(x, y, radius = 1, fromDegrees = 180, ...){
	xx <- degrees2radians(seq(fromDegrees, fromDegrees + 90, by = .5))
	x <- cos(xx) * radius + x
	y <- sin(xx) * radius + y
	lines(x, y, ...)
}

curlyBrace1 <- function(xl, y, length = 5, radius1 = .5, radius2 = .25, top = TRUE, ...){  
	# i.e. the pointy part on top or on bottom?
	if (top){
		quarterArc(xl + radius1, y - radius1, radius = radius1, fromDegrees = 90, ...)
		quarterArc(xl + length - radius1, y - radius1 , radius = radius1, fromDegrees = 0, ...)
		quarterArc(xl + length / 2 - radius2, y + radius2, radius = radius2, fromDegrees = 270, ...)
		quarterArc(xl + length / 2 + radius2, y + radius2, radius = radius2, fromDegrees = 180, ...)
	} else {
		quarterArc(xl + radius1, y + radius1, radius = radius1, fromDegrees = 180, ...)
		quarterArc(xl + length - radius1, y + radius1 , radius = radius1, fromDegrees = 0 - 90, ...)
		quarterArc(xl + length / 2 - radius2, y - radius2, radius = radius2, fromDegrees = 270 + 90, ...)
		quarterArc(xl + length / 2 + radius2, y - radius2, radius = radius2, fromDegrees = 180 - 90, ...)        
	}
	segments(xl + radius1, y, xl + length / 2 - radius2, y, ...)
	segments(xl + length - radius1, y, xl + length / 2 + radius2, y, ...)   
}

# from JS, 27-3-2017
GenerateTransformationMatrix <- function (n) {
	stopifnot(n>=2); n=n-1
	GenSubmatrx <- function (i) cbind(-diag(i), 1, matrix(0, ncol = n-i, nrow = i))
	do.call("rbind", lapply(1:n, "GenSubmatrx"))
}
#n <- 4
#make.At(4)
DefaultDurationOrdering <- function(p){
	n       <- length(p)
	At      <- GenerateTransformationMatrix(n)
	# ugly but works
	tofromi <- t(apply(At, 1, function(x){
						c(which(x == -1),  which(x == 1))
					}))
	from    <- p[tofromi[,1]]
	to      <- p[tofromi[,2]]
	
	m 		<- length(to)
	#sum(cumsum(1:(n-1)))-n
	data.frame(d = 1:m,pfrom = tofromi[,1],pto=tofromi[,2],from = from, to = to, dur = to - from)
}


draw.timeline <- function(p,ylim=c(-3,0),dcol,pcol,label=TRUE,...){
	durs <- DefaultDurationOrdering(p)
	if (missing(dcol)){
		dcol <- rep("black",nrow(durs))
	}
	if (missing(pcol)){
		pcol <- rep("red",n)
	}
	
	par(mai=c(.1,.1,.1,.1),xpd=TRUE)
	plot(NULL, xlim = range(p), ylim = ylim, axes = FALSE, xlab = "", ylab = "",asp=1)
	segments(min(p),0,max(p),0,lwd=2,col = gray(.5))
#points(p,rep(0,4),cex=2,pch=16)
	segments(p,-.08,p,.08,lwd=4,col = pcol,...)
	if (label){
	for (i in 1:length(p)){
		text(p[i],.08,substitute(p[x], list(x = i)),pos=3)
	}}
	for (i in 1:nrow(durs)){
		x <- durs$from[i]
		l <- durs$dur[i]
		y <- -i*.5
		curlyBrace1(xl = x, y = y, length = l, top = FALSE, radius1 = .1, radius2 = .08, col = dcol[i])
		if (label){
		text(x+l/2,y-.05,substitute(d[x], list(x = i)),pos=1)
	}
	}
	
}

decide.verts <- function(p){
	n      <- length(p)
	radint <- seq((2*pi),0,length=n+1)[-(n+1)] + pi / 2
	list(x=cos(radint),y=sin(radint))
}

star.timeline <- function(p,lprop=.5){
	n     <- length(p)
	verts <- decide.verts(p)
	durs  <- DefaultDurationOrdering(p)
	par(xpd=TRUE)
	plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
	
	for (i in 1:nrow(durs)){
		fr <- durs$pfrom[i]
		to <- durs$pto[i]
		d  <- to - fr
		lpropi <- ifelse(d == 1 | d == (n - 1), .5, lprop)
		
		x1 <- verts$x[fr]
		x2 <- verts$x[to]
		y1 <- verts$y[fr]
		y2 <- verts$y[to]
		segments(x1,y1,x2,y2)
		
		lx <- x1*lpropi+x2*(1-lpropi)
		ly <- y1*lpropi+y2*(1-lpropi)
		points(lx,ly,pch=22,cex=3.5,col="white",bg="white")
		text(lx,ly,substitute(d[x], list(x = i)))
	}
	for (i in 1:length(p)){
		points(verts$x[i],verts$y[i],pch=21,cex=3.5,col="red",bg="white")
		text(verts$x[i],verts$y[i],substitute(p[x], list(x = i)))
	}
}


# Author: tim
###############################################################################
# install.packages("expm")


# make ugly generalized function
# A degree-n event-duration identity is identifiable from a set of n' events and m' durations
# if the graph formed by the set of n' and m' is a spanning tree of the edge-only graph of 
# said identity. That is, the graph is connected and includes each of the vertices. 

# This is the same as demanding that the minimal graph (durations as edges, events as nodes)
# is connected and includes at least one node explicitly (anchored).

# two dependencies copied from Timelines.R
GenerateTransformationMatrix <- function (n) {
	stopifnot(n>=2); n=n-1
	GenSubmatrx <- function (i) cbind(-diag(i), 1, matrix(0, ncol = n-i, nrow = i))
	do.call("rbind", lapply(1:n, "GenSubmatrx"))
}

# this produces all durations implied by a set of events, p
# including to-from vertices (not directed), as well as duration values.
# used as utility here and there.
DefaultDurationOrdering <- function(p){
	n       <- length(p)
	At      <- GenerateTransformationMatrix(n)
	# an inane way to get 
	tofromi <- t(apply(At, 1, function(x){
						c(which(x == -1),  which(x == 1))
					}))
	from    <- p[tofromi[,1]]
	to      <- p[tofromi[,2]]
	
	m 		<- length(to)
	#sum(cumsum(1:(n-1)))-n
	data.frame(d = 1:m,pfrom = tofromi[,1],pto=tofromi[,2],from = from, to = to, dur = to - from)
}

# this generates an adjacency matrix for the n+1 edge-only identity
np_adjacency <- function(n=4, edges=c("p1","p2","d1")){
	p       				<- rep(1, n)
	n1    					<- n + 1
	# get all durations
	durs  					<- DefaultDurationOrdering(p)
	dd 	  					<- paste0("d", durs$d)
	pp    					<- paste0("p", 1:n)
	# set of all durations and events, given with vertices in edge-only graph.
	edges.all  				<- data.frame(v1 = c(durs$pfrom, 1:n),
			v2 = c(durs$pto, rep(n1,n)))
	rownames(edges.all)   	<- c(dd,pp)
	
	edges.have            	<- edges.all[edges,]
	edges.have            	<- as.matrix(edges.have)
	# make an adjacency matrix
	adj 	              	<- matrix(0, ncol = n1, nrow = n1, dimnames = list(1:n1, 1:n1))   
	adj[edges.have] 		<- 1
	adj[edges.have[,c(2,1)]] <- 1
	
	# TR: 16-4-2017 OK, we only have 1s on diag 
	# for vertices that actually are touched...?
	vdiag <- rep(0,n1)
	vhave <- unique(c(edges.have))
	vdiag[vhave] <- 1
	adj                   	<- adj + diag(vdiag)
	
	# an extra object for orientation
	edges.all             	<- as.matrix(edges.all)
	A                     	<- adj * 0
	A[edges.all]          	<- rownames(edges.all)
	A[edges.all[,c(2,1)]] 	<- rownames(edges.all)
	diag(A) 				<- paste0("v",1:n1)
	
	list(adj=adj, edgeids = A)
}

# this function expands to the full set of identifiable edges
np_expand <- function(n=4, edges=c("p1","p2","d1")){
	require(expm)
	adjs 		<- np_adjacency(n, edges)
	adj 		<- adjs$adj
	ids 		<- adjs$edgeids
	
	# build out full coinnectivity by taking nth power
	# operator from expm package
	adji 		<- adj %^% n
	
	U 			<- upper.tri(adj)
	edges.identifiable <- adji > 0 & U
	
	list(edges.have = edges, 
			edges.expanded = ids[edges.identifiable],
			edges.all = ids[U])
} 

# this is a function to determine if a set of events and durations (p_i and d_i) identifies an
# n-point identity. This works by defining the n+1-vertex graph consisting of all possible p_i and d_i,
# and then checking whether the specified set of edges connects the graph or not. This is done by 
# checking whether the symmetrix adjacency matrix is invertible or not. Returns TRUE or FALSE.

#identifiable <- function(n=4,edges=c("p1","p2","d1")){
#	adj <- np_adjacency(n, edges=edges)$adj
#	# is this matrix invertible?
#	determinant(adj,FALSE)$modulus != 0
#}
identifiable <- function(n=4,edges=c("p1","p2","d1")){
	adj <- np_adjacency(n, edges=edges)$adj
	# is this matrix invertible?
	all(adj %^% n > 0)
}


# generate
generateSpanningTrees <- function(n=3){
	# generate all indices (all p will give it
	edges.fake  <- paste0("p",1:n)
	vs    		<- 1:(n+1)
	ids   		<- np_adjacency(n=n,  edges = edges.fake)$edgeids
	# n choose 2 combos of all edges yields all vertex pairs
	edges.all 	<- ids[t(combn(vs,2))]
	# then create all sets of n vertex pairs
	np1trees 	<- combn(edges.all,n)
	# given all sets of n vertex pairs (i.e. edges), which are identifiable?
	identified 	<- apply(np1trees,2,identifiable,n=n)
	# select down
	as.list(as.data.frame(np1trees[, identified]))
}


draw.tree <- function(n=4, edges, lprop = .5, x = 0, y = 0, add = FALSE, label = TRUE, col = NULL,...){
	p 			<- rep(1,n)
	n1    		<- n + 1
	# get coords for the n+1 vertices
	verts 		<- decide.verts(c(p,1))
	# get d1...dm
	durs  		<- DefaultDurationOrdering(p)
	# need comparable name, for selecting later
	durs$name 	<- paste0("d", durs$d)
	# remove unneeded columns
	durs$from 	<- NULL
	durs$to 	<- NULL
	durs$d 		<- NULL
	durs$dur 	<- NULL
	# make same for period measures, colnames must match
	pp 			<- data.frame(pfrom = 1:n, 
			pto = rep(n1,n), 
			name = paste0("p",1:n))
	# join event and duration edges
	all.edges 	<- rbind(pp,durs)
	
	if (is.null(col)){
		col <- rep("black",nrow(all.edges))
	} 
	all.edges$col <- col
	# select those specified
	edges.draw 	<- all.edges[all.edges$name %in% edges, ]
	
	if (!add){
		# create empty device
		par(xpd=TRUE)
		plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
	}
	for (i in 1:nrow(edges.draw)){
		fr <- edges.draw$pfrom[i]
		to <- edges.draw$pto[i]
		d  <- to - fr
		lpropi <- ifelse(d == 1 | d == n, .5, lprop)
		
		x1 <- verts$x[fr] + x
		x2 <- verts$x[to] + x
		y1 <- verts$y[fr] + y
		y2 <- verts$y[to] + y
		segments(x1,y1,x2,y2,col=edges.draw$col[i],...)
		
		if (label){
			lx <- x1*lpropi+x2*(1-lpropi)
			ly <- y1*lpropi+y2*(1-lpropi)
			points(lx,ly,pch=22,cex=3.5,col=par("bg"),bg=par("bg"))
			text(lx,ly,edges.draw$name[i],col=gray(.5))
		}
	}
	
}



# TODO: add color to segments and vertices
timeline.graph <- function(n,lprop=.5,dcol,pcol,label=TRUE,...){
	p     <- rep(1,n)
	verts <- decide.verts(p)
	durs  <- DefaultDurationOrdering(p)
	if (missing(dcol)){
		dcol <- rep("black",nrow(durs))
	}
	if (missing(pcol)){
		pcol <- rep("red",n)
	}
	par(xpd=TRUE)
	plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
# plot durations
	for (i in 1:nrow(durs)){
		fr <- durs$pfrom[i]
		to <- durs$pto[i]
		d  <- to - fr
		lpropi <- ifelse(d == 1 | d == (n - 1), .5, lprop)
		
		x1 <- verts$x[fr]
		x2 <- verts$x[to]
		y1 <- verts$y[fr]
		y2 <- verts$y[to]
		segments(x1,y1,x2,y2,col=dcol[i],...)
		if (label){
			lx <- x1*lpropi+x2*(1-lpropi)
			ly <- y1*lpropi+y2*(1-lpropi)
			points(lx,ly,pch=22,cex=3.5,col="white",bg="white")
			text(lx,ly,substitute(d[x], list(x = i)))
		}
	}
	
	for (i in 1:length(p)){
		points(verts$x[i],verts$y[i],pch=21,cex=3.5,bg="white",col=pcol)
		if (label){
			text(verts$x[i],verts$y[i],substitute(p[x], list(x = i)))
		}
	}
}

nedges <- function(n){
	p <- paste0(rep("p",n),1:n)
	m <- n*(n-1)/2
	d <- paste0(rep("d",m),1:m)
	g <- c(p,d)
	g
}

# get all triad identities implied by n events
n_triads <- function(n){
	edges <- nedges(n)
	dyads <- combn(edges,2)
	dyads.expanded <- lapply(apply(dyads,2,np_expand,n=n),"[[",2)
	keep <- unlist(lapply(dyads.expanded,function(x){
				length(x) == 3
			}))
    dyads.id <- dyads.expanded[keep]
	dyads.id.red <- lapply(dyads.id, function(x){
				paste(sort(x),collapse="")
			})
	dyads.id[!duplicated(unlist(dyads.id.red ))]
}

#n_triads(5)

