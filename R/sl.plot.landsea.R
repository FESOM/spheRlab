sl.plot.landsea <-
function (plot.init.res,lon,lat,elem,col.land="grey",col.sea="white",lwd=1,lty=1,polar.bg.res=1) {
	
	projection = plot.init.res$projection
	xlim = plot.init.res$xlim
	ylim = plot.init.res$ylim
	
	# draw background (defines colour of land)
	if (projection == "lonlat") {
		rect(xlim[1]+(xlim[2]-xlim[1])/12,ylim[1]+(ylim[2]-ylim[1])/12,xlim[2]-(xlim[2]-xlim[1])/12,ylim[2]-(ylim[2]-ylim[1])/12,border=NA,col=col.land)
	} else if (projection == "polar") {
		xlim.mean = mean(xlim)
		xlim.rad = 5/12 * (xlim[2] - xlim[1])
		ylim.mean = mean(xlim)
		ylim.rad = 5/12 * (ylim[2] - ylim[1])
		lon1 = 0
		while (lon1 < 2*pi) {
			lon2 = lon1 + polar.bg.res*pi/180
			polygon(x=xlim.mean+xlim.rad*c(cos(lon1),cos(lon2),0),y=ylim.mean+ylim.rad*c(sin(lon1),sin(lon2),0),col=col.land,border=col.land,lwd=1)
			lon1 = lon2
		}
	} else if (projection == "regpoly") {
		stop("'regpoly' not yet implemented")
	}
	
	if (!is.na(col.sea)) {
		# draw each ocean element
		for (ne in 1:nrow(elem)) {
			sl.plot.polygon.qad(plot.init.res,lon[elem[ne,]],lat[elem[ne,]],col.fill=col.sea,border=TRUE,border.lwd=.1)
		}
	}
	
}
