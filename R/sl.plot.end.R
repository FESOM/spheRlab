sl.plot.end <-
function(plot.init.res, line.border=TRUE, col.border="black", precision=1, lwd.border=1, lty.border=1, do.close.device=plot.init.res$do.init.device) {
	
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
			lonrange = plot.init.res$lonrange
			latrange = plot.init.res$latrange
			rect(lonrange[1],latrange[1],lonrange[2],latrange[2],border=col.border,lty=lty.border,lwd=lwd.border)
		}
		
	} else if (projection == "mollweide") {
	  
	  if (line.border) {
	    lonrange = plot.init.res$lonrange
	    latrange = plot.init.res$latrange
	    if (diff(latrange) > precision) {
	      lat.ext = seq(latrange[1],latrange[2],precision)
	      if (tail(lat.ext,1) != latrange[2]) {lat.ext = c(lat.ext, latrange[2])}
	    } else {
	      lat.ext = latrange
	    }
	    xy = sl.proj.mollweide(lon=c(rep(lonrange,each=length(lat.ext)),lonrange[1]),
	                           lat=c(lat.ext,rev(lat.ext),latrange[1]))
	    lines(xy$x,xy$y,col=col.border,lwd=lwd.border,lty=lty.border)
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
	
		stop("projections other than 'lonlat', 'mollweide', 'polar', and 'regpoly' not yet implemented")
	
	}
	
	if (do.close.device) {dev.off()}
	
}
