sl.plot.contours <-
function (plot.init.res,contours.res,indices=NULL,col="black",lwd=1,lty=1,label.density=1,cex.labels=1) {
	
	#warning("ignoring labels (not yet implemented)")
	
	if (!is.null(indices)) {
		contours.res.orig = contours.res
		contours.res = list()
		for (i in 1:length(indices)) {
			contours.res[[i]] = contours.res.orig[[indices[i]]]
		}
	}
	
	Nlev = length(contours.res)
	col = rep(col,ceiling(Nlev/length(col)))[1:Nlev]
	lwd = rep(lwd,ceiling(Nlev/length(lwd)))[1:Nlev]
	lty = rep(lty,ceiling(Nlev/length(lty)))[1:Nlev]
	
	for (c in 1:length(contours.res)) {
		
		for (n in 1:length(contours.res[[c]]$segments)) {
			
			sl.plot.lines(plot.init.res,lon=contours.res[[c]]$segments[[n]]$lon,lat=contours.res[[c]]$segments[[n]]$lat,col=col,lwd=lwd,lty=lty)
			
		}
		
	}
	
}
