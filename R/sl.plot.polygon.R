sl.plot.polygon <-
function (plot.init.res,lon,lat,fill=TRUE,col.fill="black",border=FALSE,col.border=col.fill,border.lwd=0.01,border.lty=1,ignore.visibility=FALSE,remove.identical.neighbours=TRUE,refine.boundary=TRUE,refine.boundary.precision=1) {
	
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
			if (!is.null(pir$transform.function)) {
			  lonlat.trans = pir$transform.function(lon,lat)
			  lon = lonlat.trans$lon
			  lat = lonlat.trans$lat
			}
			sl.plot.polygon(pir,lon,lat,fill,col.fill,border,col.border,border.lwd,border.lty,ignore.visibility,remove.identical.neighbours=FALSE,refine.boundary,refine.boundary.precision)
			npir = npir + 1
		}
		
	}
		
	if (!border) {col.border=NA}
	
	if (!is.null(pir$transform.function)) {
	  lonlat.trans = pir$transform.function(lon,lat)
	  lon = lonlat.trans$lon
	  lat = lonlat.trans$lat
	}
	
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
				inds = sl.segment(x>lonlat.lonrange[1],extend=TRUE)
				if (is.list(inds)) {
					for (i in 2:length(inds)) {
						sl.plot.polygon(pir,x[inds[[i]]],y[inds[[i]]],fill,col.fill,border,col.border,border.lwd,border.lty,ignore.visibility,remove.identical.neighbours,refine.boundary,refine.boundary.precision)
					}
					x = x[inds[[1]]]
					y = y[inds[[1]]]
				} else {
					x = x[inds]
					y = y[inds]
				}
				L = length(x)
				if (x[1] < lonlat.lonrange[1]) {
					if (x[2]-x[1]>180) {
						y[1] = y[2] + (lonlat.lonrange[2] - x[2])/(x[1]+360 - x[2]) * (y[1] - y[2])
						x[1] = lonlat.lonrange[2]
					} else {
						y[1] = y[2] + (lonlat.lonrange[1] - x[2])/(x[1] - x[2]) * (y[1] - y[2])
						x[1] = lonlat.lonrange[1]
					}
				}
				if (x[L] < lonlat.lonrange[1]) {
					if (x[L-1]-x[L]>180) {
						y[L] = y[L-1] + (lonlat.lonrange[2] - x[L-1])/(x[L]+360 - x[L-1]) * (y[L] - y[L-1])
						x[L] = lonlat.lonrange[2]
					} else {
						y[L] = y[L-1] + (lonlat.lonrange[1] - x[L-1])/(x[L] - x[L-1]) * (y[L] - y[L-1])
						x[L] = lonlat.lonrange[1]
					}
				}
			}
			if (max(x) > lonlat.lonrange[2]) {
				inds = sl.segment(x<lonlat.lonrange[2],extend=TRUE)
				if (is.list(inds)) {
					for (i in 2:length(inds)) {
						sl.plot.polygon(pir,x[inds[[i]]],y[inds[[i]]],fill,col.fill,border,col.border,border.lwd,border.lty,ignore.visibility,remove.identical.neighbours,refine.boundary,refine.boundary.precision)
					}
					x = x[inds[[1]]]
					y = y[inds[[1]]]
				} else {
					x = x[inds]
					y = y[inds]
				}
				L = length(x)
				if (x[1] > lonlat.lonrange[2]) {
					if (x[1]-x[2]>180) {
						y[1] = y[2] + (lonlat.lonrange[1] - x[2])/(x[1]-360 - x[2]) * (y[1] - y[2])
						x[1] = lonlat.lonrange[1]
					} else {
						y[1] = y[2] + (lonlat.lonrange[2] - x[2])/(x[1] - x[2]) * (y[1] - y[2])
						x[1] = lonlat.lonrange[2]
					}
				}
				if (x[L] > lonlat.lonrange[2]) {
					if (x[L]-x[L-1]>180) {
						y[L] = y[L-1] + (lonlat.lonrange[1] - x[L-1])/(x[L]-360 - x[L-1]) * (y[L] - y[L-1])
						x[L] = lonlat.lonrange[1]
					} else {
						y[L] = y[L-1] + (lonlat.lonrange[2] - x[L-1])/(x[L] - x[L-1]) * (y[L] - y[L-1])
						x[L] = lonlat.lonrange[2]
					}
				}
			}
			if (min(y) < lonlat.latrange[1]) {
				inds = sl.segment(y>lonlat.latrange[1],extend=TRUE)
				if (is.list(inds)) {
					for (i in 2:length(inds)) {
						sl.plot.polygon(pir,x[inds[[i]]],y[inds[[i]]],fill,col.fill,border,col.border,border.lwd,border.lty,ignore.visibility,remove.identical.neighbours,refine.boundary,refine.boundary.precision)
					}
					x = x[inds[[1]]]
					y = y[inds[[1]]]
				} else {
					x = x[inds]
					y = y[inds]
				}
				L = length(x)
				if (y[1] < lonlat.latrange[1]) {
					if (abs(x[1]-x[2])>180) {
						if (x[1]-x[2]>180) {
							x[1]=x[1]-360
						} else {
							x[1]=x[1]+360
						}
						x[1] = x[2] + (lonlat.latrange[1] - y[2])/(y[1] - y[2]) * (x[1] - x[2])
						if (x[1]<lonlat.lonrange[1]) {x[1]=x[1]+360}
						if (x[1]>lonlat.lonrange[2]) {x[1]=x[1]-360}
						y[1] = lonlat.latrange[1]
					} else {
						x[1] = x[2] + (lonlat.latrange[1] - y[2])/(y[1] - y[2]) * (x[1] - x[2])
						y[1] = lonlat.latrange[1]
					}
				}
				if (y[L] < lonlat.latrange[1]) {
					if (abs(x[L]-x[L-1])>180) {
						if (x[L]-x[L-1]>180) {
							x[L]=x[L]-360
						} else {
							x[L]=x[L]+360
						}
						x[L] = x[L-1] + (lonlat.latrange[1] - y[L-1])/(y[L] - y[L-1]) * (x[L] - x[L-1])
						if (x[L]<lonlat.lonrange[1]) {x[L]=x[L]+360}
						if (x[L]>lonlat.lonrange[2]) {x[L]=x[L]-360}
						y[L] = lonlat.latrange[1]
					}
					else {
						x[L] = x[L-1] + (lonlat.latrange[1] - y[L-1])/(y[L] - y[L-1]) * (x[L] - x[L-1])
						y[L] = lonlat.latrange[1]
					}
				}
			}
			if (max(y) > lonlat.latrange[2]) {
				inds = sl.segment(y<lonlat.latrange[2],extend=TRUE)
				if (is.list(inds)) {
					for (i in 2:length(inds)) {
						sl.plot.polygon(pir,x[inds[[i]]],y[inds[[i]]],fill,col.fill,border,col.border,border.lwd,border.lty,ignore.visibility,remove.identical.neighbours,refine.boundary,refine.boundary.precision)
					}
					x = x[inds[[1]]]
					y = y[inds[[1]]]
				} else {
					x = x[inds]
					y = y[inds]
				}
				L = length(x)
				if (y[1] > lonlat.latrange[2]) {
					if (abs(x[1]-x[2])>180) {
						if (x[1]-x[2]>180) {
							x[1]=x[1]-360
						} else {
							x[1]=x[1]+360
						}
						x[1] = x[2] + (lonlat.latrange[2] - y[2])/(y[1] - y[2]) * (x[1] - x[2])
						if (x[1]<lonlat.lonrange[1]) {x[1]=x[1]+360}
						if (x[1]>lonlat.lonrange[2]) {x[1]=x[1]-360}
						y[1] = lonlat.latrange[2]
					} else {
						x[1] = x[2] + (lonlat.latrange[2] - y[2])/(y[1] - y[2]) * (x[1] - x[2])
						y[1] = lonlat.latrange[2]
					}
				}
				if (y[L] > lonlat.latrange[2]) {
					if (abs(x[L]-x[L-1])>180) {
						if (x[L]-x[L-1]>180) {
							x[L]=x[L]-360
						} else {
							x[L]=x[L]+360
						}
						x[L] = x[L-1] + (lonlat.latrange[2] - y[L-1])/(y[L] - y[L-1]) * (x[L] - x[L-1])
						if (x[L]<lonlat.lonrange[1]) {x[L]=x[L]+360}
						if (x[L]>lonlat.lonrange[2]) {x[L]=x[L]-360}
						y[L] = lonlat.latrange[2]
					} else {
						x[L] = x[L-1] + (lonlat.latrange[2] - y[L-1])/(y[L] - y[L-1]) * (x[L] - x[L-1])
						y[L] = lonlat.latrange[2]
					}
				}
			}
		}
		if (max(x) - min(x) > 180 && max(abs(x - x[c(2:L,1)])) > 180) {
			l2r = which(x[c(2:L,1)]-x > 180)
			r2l = which(x[c(2:L,1)]-x < -180)
			N.lr = length(l2r)
			N.rl = length(r2l)
			if (N.lr != N.rl) {
				warning("This nasty polygon can not be plotted; it might be circular, crossing the lonlat boundary an uneven number of times, that is, it may contain one or the other pole. Consider splitting the polygon into better behaving pieces.")
				return()
			}
			if (N.lr > 1) {
				if (l2r[1] > r2l[1]) {r2l = r2l[c(2:L,1)]}
				for (i in 2:N.lr) {
					if (r2l[i]%%L+1 > l2r[i]) {
						right = l2r[i]:(r2l[i]%%L+1)
					} else {
						right = c(l2r[i]:L,1:(r2l[i]%%L+1))
					}
					if (l2r[i%%N.lr+1]%%L+1 > r2l[i]) {
						left = r2l[i]:(l2r[i%%N.lr+1]%%L+1)
					} else {
						left = c(r2l[i]:L,1:(l2r[i%%N.lr+1]%%L+1))
					}
					sl.plot.polygon(pir,x[left],y[left],fill,col.fill,border,col.border,border.lwd,border.lty,ignore.visibility,remove.identical.neighbours,refine.boundary,refine.boundary.precision)
					sl.plot.polygon(pir,x[right],y[right],fill,col.fill,border,col.border,border.lwd,border.lty,ignore.visibility,remove.identical.neighbours,refine.boundary,refine.boundary.precision)
				}
			}
			if (r2l[1]%%L+1 > l2r[1]) {
				right = l2r[1]:(r2l[1]%%L+1)
			} else {
				right = c(l2r[1]:L,1:(r2l[1]%%L+1))
			}
			if (l2r[1%%N.lr+1]%%L+1 > r2l[1]) {
				left = r2l[1]:(l2r[1%%N.lr+1]%%L+1)
			} else {
				left = c(r2l[1]:L,1:(l2r[1%%N.lr+1]%%L+1))
			}
			x.r = x[right]
			y.r = y[right]
			L.r = length(right)
			if (x.r[1] < lonlat.lonrange[2]) {x.r[1] = x.r[1] + 360}
			y.r[1] = y.r[2] + (lonlat.lonrange[2] - x.r[2])/(x.r[1] - x.r[2]) * (y.r[1] - y.r[2])
			x.r[1] = lonlat.lonrange[2]
			if (x.r[L.r] < lonlat.lonrange[2]) {x.r[L.r] = x.r[L.r] + 360}
			y.r[L.r] = y.r[L.r-1] + (lonlat.lonrange[2] - x.r[L.r-1])/(x.r[L.r] - x.r[L.r-1]) * (y.r[L.r] - y.r[L.r-1])
			x.r[L.r] = lonlat.lonrange[2]
			x.l = x[left]
			y.l = y[left]
			L.l = length(left)
			if (x.l[1] > lonlat.lonrange[1]) {x.l[1] = x.l[1] - 360}
			y.l[1] = y.l[2] + (lonlat.lonrange[1] - x.l[2])/(x.l[1] - x.l[2]) * (y.l[1] - y.l[2])
			x.l[1] = lonlat.lonrange[1]
			if (x.l[L.l] > lonlat.lonrange[1]) {x.l[L.l] = x.l[L.l] - 360}
			y.l[L.l] = y.l[L.l-1] + (lonlat.lonrange[1] - x.l[L.l-1])/(x.l[L.l] - x.l[L.l-1]) * (y.l[L.l] - y.l[L.l-1])
			x.l[L.l] = lonlat.lonrange[1]
			polygon(x=x.r+xshift,y=y.r+yshift,col=col.fill,lwd=border.lwd,lty=border.lty,border=col.border)
			polygon(x=x.l+xshift,y=y.l+yshift,col=col.fill,lwd=border.lwd,lty=border.lty,border=col.border)
		} else {
			polygon(x=x+xshift,y=y+yshift,col=col.fill,lwd=border.lwd,lty=border.lty,border=col.border)
		}
	} else if (projection == "polar") {
		if (vis.partial) {
			llati.res1 = sl.line.lat.intersect(rot.lon[1:2],rot.lat[1:2],plot.init.res$polar.latbound)
			if (!llati.res1$line.lat.intersect) {
				warning("line does not intersect with the bounding latitude, something is wrong")
				return()
			}
			if (llati.res1$line.lat.intersect.twice) {
				warning("line intersects twice with the bounding latitude, something is wrong")
				return()
			}
			x[1] = llati.res1$x
			y[1] = llati.res1$y
			llati.res2 = sl.line.lat.intersect(rot.lon[(L-1):L],rot.lat[(L-1):L],plot.init.res$polar.latbound)
			if (!llati.res2$line.lat.intersect) {
				warning("line does not intersect with the bounding latitude, something is wrong")
				return()
			}
			if (llati.res2$line.lat.intersect.twice) {
				warning("line intersects twice with the bounding latitude, something is wrong")
				return()
			}
			x[L] = llati.res2$x
			y[L] = llati.res2$y
			if (refine.boundary) {
				lon1 = llati.res1$lon
				lon2 = llati.res2$lon
				if (lon2 < lon1) {
					if ((lon1-lon2) <= 180) {
						ori = 1
					} else {
						ori = -1
						lon1 = lon1 - 360
					}
				} else {
					if ((lon2-lon1) <= 180) {
						ori = -1
					} else {
						ori = 1
						lon1 = lon1 + 360
					}
				}
				Nrefine = floor(abs(lon2-lon1))
				if (Nrefine > 0) {
					lons.ext = seq(lon2,lon1,by=ori*refine.boundary.precision)
					x.ext = c()
					y.ext = c()
					for (i in 1:Nrefine) {
						xyz = sl.lonlat2xyz(c(lons.ext[i],plot.init.res$polar.latbound))
						x.ext = c(x.ext,xyz[1])
						y.ext = c(y.ext,xyz[2])
					}
					x = c(x,x.ext)
					y = c(y,y.ext)
				}
			}
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
