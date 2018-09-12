sl.plot.end <-
function(plot.init.res, line.border=TRUE, col.border="black", precision=1, lwd.border=1, lty.border=1, do.close.device=TRUE) {
	
	projection = plot.init.res$projection
	
	if (projection == "polar") {
		
		if (line.border) {
			xyrad = sin(pi*(90-plot.init.res$polar.latbound)/180)
			x = c(-cos(seq(0,2*pi,2*pi*precision/360))) * xyrad
			y = c(sin(seq(0,2*pi,2*pi*precision/360))) * xyrad
			lines(x,y,col=col.border,lwd=lwd.border,lty=lty.border)
		}
	
	} else if (projection == "lonlat") {
		
		if (line.border) {
			lonlat.lonrange = plot.init.res$lonlat.lonrange
			lonlat.latrange = plot.init.res$lonlat.latrange
			rect(lonlat.lonrange[1],lonlat.latrange[1],lonlat.lonrange[2],lonlat.latrange[2],border=col.border,lty=lty.border,lwd=lwd.border)
		}
		
	} else if (projection == "regpoly") {
		
		if (line.border) {
			regpoly.cornerlons = plot.init.res$regpoly.cornerlons
			regpoly.cornerlats = plot.init.res$regpoly.cornerlats
			lon.ext = c(regpoly.cornerlons,regpoly.cornerlons[1])
			lat.ext = c(regpoly.cornerlats,regpoly.cornerlats[1])
			sl.plot.lines(plot.init.res,lon.ext,lat.ext,col=col.border,lwd=lwd.border,lty=lty.border,ignore.visibility=TRUE)
		}
	
	} else {
	
		stop("projections other than 'lonlat', 'polar', and 'regpoly' not yet implemented")
	
	}
	
	if (do.close.device) {dev.off()}
	
}
