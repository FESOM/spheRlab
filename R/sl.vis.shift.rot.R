sl.vis.shift.rot <-
function (plot.init.res,lon,lat) {

	projection = plot.init.res$projection
	rot.lon = NULL
	rot.lat = NULL

	# determine visibility, shift longitudes (if necessary), and rotate (if necessary)
	if (projection == "lonlat") {
		lonlat.lonrange = plot.init.res$lonlat.lonrange
		lonlat.latrange = plot.init.res$lonlat.latrange
		while (min(lon) < -180) {
			lon[lon < -180] = lon[lon < -180] + 360
		}
		while (max(lon) > 180) {
			lon[lon > 180] = lon[lon > 180] - 360
		}
		if (min(lon)+360 < lonlat.lonrange[2]) {
			lon[lon<lonlat.lonrange[1]] = lon[lon<lonlat.lonrange[1]] + 360
		} else if (max(lon)-360 > lonlat.lonrange[1]) {
			lon[lon>lonlat.lonrange[2]] = lon[lon>lonlat.lonrange[2]] - 360
		}
		visible = (lat>=lonlat.latrange[1] & lat<=lonlat.latrange[2] & lon>=lonlat.lonrange[1] & lon<=lonlat.lonrange[2])
		x = lon
		y = lat
	} else if (projection == "polar") {
		alpha = plot.init.res$alpha
		beta = plot.init.res$beta
		gamma = plot.init.res$gamma
		rot.res = sl.rot(lon,lat,alpha,beta,gamma,return.xyz=TRUE)
		rot.lon = rot.res$lon
		rot.lat = rot.res$lat
		visible = (rot.lat >= plot.init.res$polar.latbound)
		x = rot.res$x
		y = rot.res$y
	} else if (projection == "regpoly") {
		alpha = plot.init.res$alpha
		beta = plot.init.res$beta
		gamma = plot.init.res$gamma
		regpoly.lat0 = plot.init.res$regpoly.lat0
		regpoly.lat0i = plot.init.res$regpoly.lat0i
		regpoly.cornerlons0 = plot.init.res$regpoly.cornerlons0
		regpoly.N = plot.init.res$regpoly.N
		regpoly.z0 = plot.init.res$regpoly.z0
		rot.res = sl.rot(lon,lat,alpha,beta,gamma,return.xyz=TRUE)
		rot.lon = rot.res$lon
		rot.lat = rot.res$lat
		visible = (rot.lat >= regpoly.lat0)
		for (n in which(visible & (rot.lat < regpoly.lat0i))) {
			visible[n] = sl.inpolygon.convex(rot.lon[n],rot.lat[n],regpoly.cornerlons0,rep(regpoly.lat0,regpoly.N),check.ccw.convex=FALSE)
			#if (!visible[n]) {print("discarding point from visibility")}
		}
		x = rep(NA,length(rot.res$x))
		y = x
		pos.hem = which(rot.res$z > 0)
		stretch.fac = x
		stretch.fac[pos.hem] = regpoly.z0 / rot.res$z[pos.hem]
		x[pos.hem] = rot.res$x[pos.hem] * stretch.fac[pos.hem]
		y[pos.hem] = rot.res$y[pos.hem] * stretch.fac[pos.hem]
	} else {
		stop("projections other than 'lonlat', 'polar', and 'regpoly' not yet implemented")
	}
	
	return(list(x=x,y=y,rot.lon=rot.lon,rot.lat=rot.lat,visible=visible))
	
}
