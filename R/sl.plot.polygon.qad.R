sl.plot.polygon.qad <-
function (plot.init.res,lon,lat,fill=TRUE,col.fill="black",border=FALSE,col.border=col.fill,border.lwd=0.01,border.lty=1,ignore.visibility=FALSE,remove.identical.neighbours=TRUE) {
	
	L = length(lon)
	if (remove.identical.neighbours) {
		lon.shift = c(lon[2:L],lon[1])
		lat.shift = c(lat[2:L],lat[1])
		keep = !(sl.lonlat.identical(lon,lat,lon.shift,lat.shift))
		lon = lon[keep]
		lat = lat[keep]
		L = length(lon)
	}
	
	projection = plot.init.res$projection
	
	if (projection == "platon" || projection == "3D") {
		
		npir = 1
		repeat {
			pir = plot.init.res[[npir]]
			if (!is.list(pir)) {return()}
			if (is.null(pir$projection)) {return()}
			#print(paste("plotting subplot",npir,"...",sep=" "))
			sl.plot.polygon.qad(pir,lon,lat,fill,col.fill,border,col.border,border.lwd,border.lty,ignore.visibility,remove.identical.neighbours=FALSE)
			npir = npir + 1
		}
		
	}
		
	if (!border) {col.border=NA}
	
	vsr.res = sl.vis.shift.rot(plot.init.res,lon,lat)
	visible = vsr.res$visible
	if (ignore.visibility) {visible[] = TRUE}
	if (sum(visible) == 0) {return()}
	x = vsr.res$x
	y = vsr.res$y
	rot.lon = vsr.res$rot.lon
	rot.lat = vsr.res$rot.lat
	
	vis.partial = FALSE
	if (sum(visible) < L) {
		vis.partial = TRUE
	}
		
	if (vis.partial && projection != "lonlat") {
		visible.ext = c(visible,visible[1])
		x.new = NULL
		y.new = NULL
		rot.lon.new = NULL
		rot.lat.new = NULL
		i = 0
		repeat {
			i = i + 1
			if (!visible.ext[i] && visible.ext[i+1]) {break}
		}
		repeat {
			x.new = c(x.new,x[i])
			y.new = c(y.new,y[i])
			rot.lon.new = c(rot.lon.new,rot.lon[i])
			rot.lat.new = c(rot.lat.new,rot.lat[i])
			i = (i%%L) + 1
			if (visible.ext[i] && !visible.ext[i+1]) {break}
		}
		x.new = c(x.new,x[i])
		y.new = c(y.new,y[i])
		rot.lon.new = c(rot.lon.new,rot.lon[i])
		rot.lat.new = c(rot.lat.new,rot.lat[i])
		i = (i%%L) + 1
		x = c(x.new,x[i])
		y = c(y.new,y[i])
		rot.lon = c(rot.lon.new,rot.lon[i])
		rot.lat = c(rot.lat.new,rot.lat[i])
		L = length(x)
		rm(x.new,y.new,rot.lon.new,rot.lat.new)
	}
	
	xshift = plot.init.res$xshift
	yshift = plot.init.res$yshift
		
	if (projection == "lonlat") {
		lonlat.lonrange = plot.init.res$lonlat.lonrange
		lonlat.latrange = plot.init.res$lonlat.latrange
		if (vis.partial) {
			if (min(x) < lonlat.lonrange[1]) {
				inds = sl.segment(x>lonlat.lonrange[1],extend=TRUE,first.only=TRUE)
				x = x[inds]
				y = y[inds]
				L = length(x)
				if (x[1] < lonlat.lonrange[1]) {
					y[1] = y[2] + (lonlat.lonrange[1] - x[2])/(x[1] - x[2]) * (y[1] - y[2])
					x[1] = lonlat.lonrange[1]
				}
				if (x[L] < lonlat.lonrange[1]) {
					y[L] = y[L-1] + (lonlat.lonrange[1] - x[L-1])/(x[L] - x[L-1]) * (y[L] - y[L-1])
					x[L] = lonlat.lonrange[1]
				}
			}
			if (max(x) > lonlat.lonrange[2]) {
				inds = sl.segment(x<lonlat.lonrange[2],extend=TRUE,first.only=TRUE)
				x = x[inds]
				y = y[inds]
				L = length(x)
				if (x[1] > lonlat.lonrange[2]) {
					y[1] = y[2] + (lonlat.lonrange[2] - x[2])/(x[1] - x[2]) * (y[1] - y[2])
					x[1] = lonlat.lonrange[2]
				}
				if (x[L] > lonlat.lonrange[2]) {
					y[L] = y[L-1] + (lonlat.lonrange[2] - x[L-1])/(x[L] - x[L-1]) * (y[L] - y[L-1])
					x[L] = lonlat.lonrange[2]
				}
			}
			if (min(y) < lonlat.latrange[1]) {
				inds = sl.segment(y>lonlat.latrange[1],extend=TRUE,first.only=TRUE)
				x = x[inds]
				y = y[inds]
				L = length(x)
				if (y[1] < lonlat.latrange[1]) {
					x[1] = x[2] + (lonlat.latrange[1] - y[2])/(y[1] - y[2]) * (x[1] - x[2])
					y[1] = lonlat.latrange[1]
				}
				if (y[L] < lonlat.latrange[1]) {
					x[L] = x[L-1] + (lonlat.latrange[1] - y[L-1])/(y[L] - y[L-1]) * (x[L] - x[L-1])
					y[L] = lonlat.latrange[1]
				}
			}
			if (max(y) > lonlat.latrange[2]) {
				inds = sl.segment(y<lonlat.latrange[2],extend=TRUE,first.only=TRUE)
				x = x[inds]
				y = y[inds]
				L = length(x)
				if (y[1] > lonlat.latrange[2]) {
					x[1] = x[2] + (lonlat.latrange[2] - y[2])/(y[1] - y[2]) * (x[1] - x[2])
					y[1] = lonlat.latrange[2]
				}
				if (y[L] > lonlat.latrange[2]) {
					x[L] = x[L-1] + (lonlat.latrange[2] - y[L-1])/(y[L] - y[L-1]) * (x[L] - x[L-1])
					y[L] = lonlat.latrange[2]
				}
			}
		}
		if (max(x) - min(x) > 180) {
			# this seems to be a circular boundary polygon that needs to be drawn in two parts on both sides
			left = (x < mean(lonlat.lonrange))
			left.ext = c(left,left[1])
			x.left = NULL
			y.left = NULL
			x.right = NULL
			y.right = NULL
			i = 0
			repeat {
				i = i + 1
				if (!left.ext[i] && left.ext[i+1]) {break}
			}
			repeat {
				x.left = c(x.left,x[i])
				y.left = c(y.left,y[i])
				i = (i%%L) + 1
				if (left.ext[i] && !left.ext[i+1]) {break}
			}
			x.left = c(x.left,x[i],x[(i%%L)+1])
			y.left = c(y.left,y[i],y[(i%%L)+1])
			L.left = length(x.left)
			lli.res = sl.line.line.intersect(x.left[1:2],y.left[1:2],rep(lonlat.lonrange[1],2),c(-89,89))
			x.left[1] = lonlat.lonrange[1]
			y.left[1] = lli.res$lat
			lli.res = sl.line.line.intersect(x.left[(L.left-1):L.left],y.left[(L.left-1):L.left],rep(lonlat.lonrange[1],2),c(-89,89))
			x.left[L] = lonlat.lonrange[1]
			y.left[L] = lli.res$lat
			polygon(x=x.right+xshift,y=y.right+yshift,col=col.fill,lwd=border.lwd,lty=border.lty,border=col.border)
			repeat {
				x.right = c(x.right,x[i])
				y.right = c(y.right,y[i])
				i = (i%%L) + 1
				if (!left.ext[i] && left.ext[i+1]) {break}
			}
			x.right = c(x.right,x[i],x[(i%%L)+1])
			y.right = c(y.right,y[i],y[(i%%L)+1])
			L.right = length(x.right)
			lli.res = sl.line.line.intersect(x.right[1:2],y.right[1:2],rep(lonlat.lonrange[2],2),c(-89,89))
			x.right[1] = lonlat.lonrange[2]
			y.right[1] = lli.res$lat
			lli.res = sl.line.line.intersect(x.right[(L.right-1):L.right],y.right[(L.right-1):L.right],rep(lonlat.lonrange[2],2),c(-89,89))
			x.right[L] = lonlat.lonrange[2]
			y.right[L] = lli.res$lat
			polygon(x=x.right+xshift,y=y.right+yshift,col=col.fill,lwd=border.lwd,lty=border.lty,border=col.border)
		} else {
			polygon(x=x+xshift,y=y+yshift,col=col.fill,lwd=border.lwd,lty=border.lty,border=col.border)
		}
	} else if (projection == "polar") {
		if (vis.partial) {
			llati.res = sl.line.lat.intersect(rot.lon[1:2],rot.lat[1:2],plot.init.res$polar.latbound)
			if (!llati.res$line.lat.intersect) {
				warning("line does not intersect with the bounding latitude, something is wrong")
				return()
			}
			if (llati.res$line.lat.intersect.twice) {
				warning("line intersects twice with the bounding latitude, something is wrong")
				return()
			}
			x[1] = llati.res$x
			y[1] = llati.res$y
			llati.res = sl.line.lat.intersect(rot.lon[(L-1):L],rot.lat[(L-1):L],plot.init.res$polar.latbound)
			if (!llati.res$line.lat.intersect) {
				warning("line does not intersect with the bounding latitude, something is wrong")
				return()
			}
			if (llati.res$line.lat.intersect.twice) {
				warning("line intersects twice with the bounding latitude, something is wrong")
				return()
			}
			x[L] = llati.res$x
			y[L] = llati.res$y
		}
		polygon(x=x+xshift,y=y+yshift,col=col.fill,lwd=border.lwd,lty=border.lty,border=col.border)
	} else if (projection == "regpoly") {
		if (vis.partial) {
			regpoly.cornerlons0 = plot.init.res$regpoly.cornerlons0
			regpoly.lat0 = plot.init.res$regpoly.lat0
			regpoly.N = plot.init.res$regpoly.N
			lat0.rep = rep(regpoly.lat0,regpoly.N)
			regpoly.z0 = plot.init.res$regpoly.z0
			lpi.res = sl.line.polygon.intersect(rot.lon[1:2],rot.lat[1:2],regpoly.cornerlons0,lat0.rep)
			if (!lpi.res$anylines.intersect) {
				warning("line does not intersect with polygon boundary, something is wrong")
				return()
			}
			if (sum(lpi.res$lines.intersect) > 1) {
				if (!any(sl.lonlat.identical(rot.lon[2],rot.lat[2],regpoly.cornerlons0,regpoly.lat0,tolerance=10^(-10)))) {
					warning("line intersects with more than one segment of the polygon boundary, and this is no corner issue; something is wrong")
					return()
				}
			}
			p.xyz = c(lpi.res$x[lpi.res$lines.intersect][1],lpi.res$y[lpi.res$lines.intersect][1],lpi.res$z[lpi.res$lines.intersect][1])
			stretch.fac = regpoly.z0 / p.xyz[3]
			x[1] = p.xyz[1] * stretch.fac
			y[1] = p.xyz[2] * stretch.fac
			lpi.res = sl.line.polygon.intersect(rot.lon[(L-1):L],rot.lat[(L-1):L],regpoly.cornerlons0,lat0.rep)
			if (!lpi.res$anylines.intersect) {
				warning("line does not intersect with polygon boundary, something is wrong")
				return()
			}
			if (sum(lpi.res$lines.intersect) > 1) {
				if (!any(sl.lonlat.identical(rot.lon[L-1],rot.lat[L-1],regpoly.cornerlons0,regpoly.lat0,tolerance=10^(-10)))) {
					warning("line intersects with more than one segment of the polygon boundary, and this is no corner issue; something is wrong")
					return()
				}
			}
			p.xyz = c(lpi.res$x[lpi.res$lines.intersect][1],lpi.res$y[lpi.res$lines.intersect][1],lpi.res$z[lpi.res$lines.intersect][1])
			stretch.fac = regpoly.z0 / p.xyz[3]
			x[L] = p.xyz[1] * stretch.fac
			y[L] = p.xyz[2] * stretch.fac
		}
		polygon(x=x+xshift,y=y+yshift,col=col.fill,lwd=border.lwd,lty=border.lty,border=col.border)
	} else {
		stop("projections other than 'lonlat', 'polar', and 'regpoly' not yet implemented")
	}
	
}
