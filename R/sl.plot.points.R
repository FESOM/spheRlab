sl.plot.points <-
function (plot.init.res,lon,lat,col="black",pch=1,cex=1,ignore.visibility=FALSE) {
	
	projection = plot.init.res$projection
	
	if (projection == "platon" || projection == "3D") {
		
		npir = 1
		repeat {
			pir = plot.init.res[[npir]]
			if (!is.list(pir)) {return()}
			if (is.null(pir$projection)) {return()}
			sl.plot.points(pir,lon,lat,col,pch,cex,ignore.visibility)
			npir = npir + 1
		}
		
	}
			
	vsr.res = sl.vis.shift.rot(plot.init.res,lon,lat)
	x = vsr.res$x
	y = vsr.res$y
	rot.lon = vsr.res$rot.lon
	rot.lat = vsr.res$rot.lat
	visible = vsr.res$visible
	
	if (ignore.visibility) {
		visible[] = TRUE
	}
	
	xshift = plot.init.res$xshift
	yshift = plot.init.res$yshift
	
	np = length(lon)
	if (sum(visible) < np) {
		if (length(col) > 1) {
			if (length(col) < np) {
				col = rep(col,np%/%length(col)+1)
			}
			col = col[(1:np)[visible]]
		}
		if (length(pch) > 1) {
			if (length(pch) < np) {
				pch = rep(pch,np%/%length(pch)+1)
			}
			pch = pch[(1:np)[visible]]
		}
		if (length(cex) > 1) {
			if (length(cex) < np) {
				cex = rep(cex,np%/%length(cex)+1)
			}
			cex = cex[(1:np)[visible]]
		}
		x = x[visible]
		y = y[visible]
	}
	
	points(x=x+xshift,y=y+yshift,col=col,pch=pch,cex=cex)
	
}
