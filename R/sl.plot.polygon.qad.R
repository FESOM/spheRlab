sl.plot.polygon.qad <-
function (plot.init.res,lon,lat,fill=TRUE,col.fill="black",border=FALSE,col.border=col.fill,border.lwd=0.01,border.lty=1,ignore.visibility=FALSE,remove.identical.neighbours=TRUE,splitside="none") {
	
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
			sl.plot.polygon.qad(pir,lon,lat,fill,col.fill,border,col.border,border.lwd,border.lty,ignore.visibility,remove.identical.neighbours=FALSE)#,border.visborder
			npir = npir + 1
		}
		
	}
	
	#if (!fill && border && !border.visborder) {
	#	sl.plot.lines(plot.init.res,lon=c(lon,lon[1]),lat=c(lat,lat[1]),col=col.border,lwd=border.lwd,lty=border.lty,ignore.visibility=ignore.visibility)
	#} else {
		
	if (!border) {col.border=NA}
	
	vsr.res = sl.vis.shift.rot(plot.init.res,lon,lat)
	visible = vsr.res$visible
	if (ignore.visibility) {visible[] = TRUE}
	if (sum(visible) == 0) {return()}
	x = vsr.res$x
	y = vsr.res$y
	rot.lon = vsr.res$rot.lon
	rot.lat = vsr.res$rot.lat
	
	#if (anyNA(x)) {return()}
	
	if (sum(visible) < L) {
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
		vis.partial = TRUE
	} else {
		vis.partial = FALSE
	}
	
	#This block may be needed for a fully-grown polygon plotting function
	#polys = list()
	#if (sum(visible) == L) {
	#	polys[[1]] = list(ind=1:L,boundary.ind=rep(NA,L))
	#	all.vis = TRUE
	#} else {
	#	all.vis = FALSE
	#	p.count = 0
	#	cont = FALSE
	#	if (visible[1]) {cont = TRUE}
	#	for (i in 2:L) {
	#		if (visible[i]) {
	#			if (!visible[i-1]) {
	#				p.count = p.count + 1
	#			}
	#			polys[[p.count]] = i
	#		}
	#	}
	#}
	
	xshift = plot.init.res$xshift
	yshift = plot.init.res$yshift
		
	if (projection == "lonlat") {
		lonlat.lonrange = plot.init.res$lonlat.lonrange
		if (splitside == "none") {
			# need to check if this is a circular polygon that needs to be drawn on both sides, i.e. twice
			if (min(x) < lonlat.lonrange[1]) {
				boundline.lon = lonlat.lonrange[1]
			} else if (max(seg.x) > lonlat.lonrange[2]) {
				boundline.lon = lonlat.lonrange[2]
			}
		} else {
			if (lonlat.lonrange[2] - lonlat.lonrange[2] == 360) {
				if (splitside == "left") {
			
				} else {
			
				}
			}
			else {
				
			}
		}
		if (vis.partial) {
			if (min(seg.x) < lonlat.lonrange[1]) {
				boundline.lon = lonlat.lonrange[1]
			} else if (max(seg.x) > lonlat.lonrange[2]) {
				boundline.lon = lonlat.lonrange[2]
			} else {stop("line seemingly completely visible, something is wrong")}
			lli.res = sl.line.line.intersect(seg.x,seg.y,rep(boundline.lon,2),c(-30,30))
			p.x = lli.res$lon
			p.y = lli.res$lat
			if (visible[ns]) {
				seg.x[1] = p.x
				seg.y[1] = p.y
			} else {
				seg.x[2] = p.x
				seg.y[2] = p.y
			}
			lines(x=seg.x+xshift,y=seg.y+yshift,col=col[ns-1],lwd=lwd,lty=lty)
		} else if (max(seg.x) - min(seg.x) > 180) {
			# this seems to be a circular boundary polygon that needs to be drawn on both sides, i.e. twice
			seg.x.mod = seg.x
			seg.x.mod[seg.x<mean(lonlat.lonrange)] = lonlat.lonrange[2]
			seg.y.mod = seg.y
			lli.res.right = sl.line.line.intersect(seg.x,seg.y,rep(lonlat.lonrange[2],2),c(-30,30))
			seg.y.mod[seg.x<mean(lonlat.lonrange)] = lli.res.right$lat
			lines(x=seg.x.mod+xshift,y=seg.y.mod+yshift,col=col[ns-1],lwd=lwd,lty=lty)
			seg.x.mod = seg.x
			seg.x.mod[seg.x>mean(lonlat.lonrange)] = lonlat.lonrange[1]
			seg.y.mod = seg.y
			lli.res.left = sl.line.line.intersect(seg.x,seg.y,rep(lonlat.lonrange[1],2),c(-30,30))
			seg.y.mod[seg.x>mean(lonlat.lonrange)] = lli.res.left$lat
			lines(x=seg.x.mod+xshift,y=seg.y.mod+yshift,col=col[ns-1],lwd=lwd,lty=lty)
		} else {
			lines(x=seg.x+xshift,y=seg.y+yshift,col=col[ns-1],lwd=lwd,lty=lty)
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
		x=x+xshift
		y=y+yshift
		polygon(x=x,y=y,col=col.fill,lwd=border.lwd,lty=border.lty,border=col.border)
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
		x=x+xshift
		y=y+yshift
		polygon(x=x,y=y,col=col.fill,lwd=border.lwd,lty=border.lty,border=col.border)
	} else {
		stop("projections other than 'lonlat', 'polar', and 'regpoly' not yet implemented")
	}
	
	#}
	
}
