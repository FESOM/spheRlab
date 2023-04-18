sl.plot.lines <-
function (plot.init.res,lon,lat,col="black",lwd=1,lty=1,ignore.visibility=FALSE) {
	
	projection = plot.init.res$projection
	
	if (projection == "platon" || projection == "3D") {
	  
	  if (!is.null(plot.init.res$transform.function)) {
	    lonlat.trans = plot.init.res$transform.function(lon,lat)
	    lon = lonlat.trans$lon
	    lat = lonlat.trans$lat
	  }
	  
		npir = 1
		repeat {
			pir = plot.init.res[[npir]]
			if (!is.list(pir)) {return()}
			if (is.null(pir$projection)) {return()}
			sl.plot.lines(pir,lon,lat,col,lwd,lty,ignore.visibility)
			npir = npir + 1
		}
		
	}
	
	if (!is.null(plot.init.res$transform.function)) {
	  lonlat.trans = plot.init.res$transform.function(lon,lat)
	  lon = lonlat.trans$lon
	  lat = lonlat.trans$lat
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
		if (projection %in% c("lonlat","mollweide")) {
			lonrange = plot.init.res$lonrange
			latrange = plot.init.res$latrange
			if (sum(visible[c(ns-1,ns)]) == 1) {
				if (min(seg.x) < lonrange[1] || max(seg.x) > lonrange[2]) {
					if (min(seg.x) < lonrange[1]) {
						boundline.lon = lonrange[1]
					} else {
						boundline.lon = lonrange[2]
					}
					p.x = boundline.lon
					p.y = sl.line.line.intersect(seg.x,seg.y,rep(boundline.lon,2),c(-89,89))$lat
				} else if (min(seg.y) < latrange[1] || max(seg.y) > latrange[2]) {
					if (min(seg.y) < latrange[1]) {
						boundline.lat = latrange[1]
					} else {
						boundline.lat = latrange[2]
					}
					p.x = sl.line.lat.intersect(seg.x,seg.y,boundline.lat)$lon
					p.y = boundline.lat
				} else {stop("line seemingly completely visible, something is wrong")}
				if (visible[ns]) {
					seg.x[1] = p.x
					seg.y[1] = p.y
				} else {
					seg.x[2] = p.x
					seg.y[2] = p.y
				}
			}
			if (max(seg.x) - min(seg.x) > 180) {
				# this seems to be a circular boundary segment that needs to be drawn in two parts on both sides
				seg.x.mod = seg.x
				seg.x.mod[seg.x<mean(lonrange)] = lonrange[2]
				seg.y.mod = seg.y
				lli.res.right = sl.line.line.intersect(seg.x,seg.y,rep(lonrange[2],2),c(-89,89))
				seg.y.mod[seg.x<mean(lonrange)] = lli.res.right$lat
				if (projection == "mollweide") {
				  seg.xy.mod = sl.proj.mollweide(lon = seg.x.mod, lat = seg.y.mod)
				  seg.x.mod = seg.xy.mod$x
				  seg.y.mod = seg.xy.mod$y
				}
				lines(x=seg.x.mod+xshift,y=seg.y.mod+yshift,col=col[ns-1],lwd=lwd,lty=lty)
				seg.x.mod = seg.x
				seg.x.mod[seg.x>mean(lonrange)] = lonrange[1]
				seg.y.mod = seg.y
				lli.res.left = sl.line.line.intersect(seg.x,seg.y,rep(lonrange[1],2),c(-89,89))
				seg.y.mod[seg.x>mean(lonrange)] = lli.res.left$lat
				if (projection == "mollweide") {
				  seg.xy.mod = sl.proj.mollweide(lon = seg.x.mod, lat = seg.y.mod)
				  seg.x.mod = seg.xy.mod$x
				  seg.y.mod = seg.xy.mod$y
				}
				lines(x=seg.x.mod+xshift,y=seg.y.mod+yshift,col=col[ns-1],lwd=lwd,lty=lty)
			} else {
			  if (projection == "mollweide") {
			    seg.xy = sl.proj.mollweide(lon = seg.x, lat = seg.y)
			    seg.x = seg.xy$x
			    seg.y = seg.xy$y
			  }
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
			stop("projections other than 'lonlat', 'mollweide', 'polar', and 'regpoly' not yet implemented")
		}
		
	}
	
}
