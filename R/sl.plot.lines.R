sl.plot.lines <-
function (plot.init.res,lon,lat,col="black",lwd=1,lty=1,ignore.visibility=FALSE) {
	
	projection = plot.init.res$projection
	
	if (projection == "platon" || projection == "3D") {
		
		npir = 1
		repeat {
			pir = plot.init.res[[npir]]
			if (!is.list(pir)) {return()}
			if (is.null(pir$projection)) {return()}
			sl.plot.lines(pir,lon,lat,col,lwd,lty,ignore.visibility)
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
	
	ns = length(lon)
	if (length(col) < (ns-1)) {
		col = rep(col,(ns-1)%/%length(col)+1)
		col = col[1:(ns-1)]
	}
	
	# draw each line segment
	for (ns in 2:length(lon)) {
		if (sum(is.na(x[c(ns-1,ns)])) > 0) {next}
		if (sum(visible[c(ns-1,ns)]) == 0) {next}
		seg.x = x[c(ns-1,ns)]
		seg.y = y[c(ns-1,ns)]
		if (projection == "lonlat") {
			lonlat.lonrange = plot.init.res$lonlat.lonrange
			if (sum(visible[c(ns-1,ns)]) == 1) {
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
				# this seems to be a circular boundary segment that needs to be drawn on both sides, i.e. twice
				seg.x.mod = seg.x
				#seg.x.mod[seg.x<mean(lonlat.lonrange)] = seg.x.mod[seg.x<mean(lonlat.lonrange)] + 360
				seg.x.mod[seg.x<mean(lonlat.lonrange)] = lonlat.lonrange[2]
				seg.y.mod = seg.y
				lli.res.right = sl.line.line.intersect(seg.x,seg.y,rep(lonlat.lonrange[2],2),c(-30,30))
				seg.y.mod[seg.x<mean(lonlat.lonrange)] = lli.res.right$lat
				lines(x=seg.x.mod+xshift,y=seg.y.mod+yshift,col=col[ns-1],lwd=lwd,lty=lty)
				#seg.x.mod = seg.x.mod - 360
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
			if (sum(visible[c(ns-1,ns)]) == 1) {
				llati.res = sl.line.lat.intersect(rot.lon[c(ns-1,ns)],rot.lat[c(ns-1,ns)],plot.init.res$polar.latbound)
				if (!llati.res$line.lat.intersect) {
					warning("line does not intersect with the bounding latitude, something is wrong")
					next
				}
				if (llati.res$line.lat.intersect.twice) {
					warning("line intersects twice with the bounding latitude, something is wrong")
					next
				}
				p.x = llati.res$x
				p.y = llati.res$y
				if (visible[ns]) {
					seg.x[1] = p.x
					seg.y[1] = p.y
				} else {
					seg.x[2] = p.x
					seg.y[2] = p.y
				}
			}
			lines(x=seg.x+xshift,y=seg.y+yshift,col=col[ns-1],lwd=lwd,lty=lty)
		} else if (projection == "regpoly") {
			if (sum(visible[c(ns-1,ns)]) == 1) {
				lpi.res = sl.line.polygon.intersect(rot.lon[c(ns-1,ns)],rot.lat[c(ns-1,ns)],plot.init.res$regpoly.cornerlons0,rep(plot.init.res$regpoly.lat0,plot.init.res$regpoly.N))
				if (!lpi.res$anylines.intersect) {
					warning("line does not intersect with the bounding latitude, something is wrong")
					next
				}
				if (sum(lpi.res$lines.intersect) > 1) {
					warning("line intersects with more than one segment of the polygon boundary, something is wrong")
					next
				}
				#p.xyz = sl.lonlat2xyz(c(lpi.res$lon[lpi.res$lines.intersect],lpi.res$lat[lpi.res$lines.intersect]))
				p.xyz = c(lpi.res$x[lpi.res$lines.intersect],lpi.res$y[lpi.res$lines.intersect],lpi.res$z[lpi.res$lines.intersect])
				stretch.fac = plot.init.res$regpoly.z0 / p.xyz[3]
				p.x = p.xyz[1] * stretch.fac
				p.y = p.xyz[2] * stretch.fac
				if (visible[ns]) {
					seg.x[1] = p.x
					seg.y[1] = p.y
				} else {
					seg.x[2] = p.x
					seg.y[2] = p.y
				}
			}
			lines(x=seg.x+xshift,y=seg.y+yshift,col=col[ns-1],lwd=lwd,lty=lty)
		} else {
			stop("projections other than 'lonlat', 'polar', and 'regpoly' not yet implemented")
		}
		
	}
	
}
