sl.vis.shift.rot <-
function (plot.init.res,lon,lat) {

	projection = plot.init.res$projection
	rot.lon = NULL
	rot.lat = NULL
	
	Norig = NULL
	if (anyNA(lon) || anyNA(lat)) {
	  Norig = length(lon)
	  which.notna = which(!(is.na(lon) | is.na(lat)))
	  lon = lon[which.notna]
	  lat = lat[which.notna]
	}

	# determine visibility, shift longitudes (if necessary), and rotate (if necessary)
	if (projection %in% c("lonlat","mollweide")) {
		lonrange = plot.init.res$lonrange
		latrange = plot.init.res$latrange
		if (!is.null(plot.init.res$lonlatrot)) {
		  alpha = plot.init.res$alpha
		  beta = plot.init.res$beta
		  gamma = plot.init.res$gamma
		  rot.res = sl.rot(lon,lat,alpha,beta,gamma,return.xyz=FALSE)
		  rot.lon = rot.res$lon
		  rot.lat = rot.res$lat
		  lon = rot.lon
		  lat = rot.lat
		}
		while (min(lon) < -180) {
			lon[lon < -180] = lon[lon < -180] + 360
		}
		while (max(lon) > 180) {
			lon[lon > 180] = lon[lon > 180] - 360
		}
		if (min(lon)+360 < lonrange[2]) {
			lon[lon<lonrange[1]] = lon[lon<lonrange[1]] + 360
		} else if (max(lon)-360 > lonrange[1]) {
			lon[lon>lonrange[2]] = lon[lon>lonrange[2]] - 360
		}
		visible = (lat>=latrange[1] & lat<=latrange[2] & lon>=lonrange[1] & lon<=lonrange[2])
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
		stop("projections other than 'lonlat', 'mollweide', 'polar', and 'regpoly' not yet implemented")
	}
	
	if (!is.null(Norig)) {
	  x.nona = x
	  y.nona = y
	  visible.nona = visible
	  x = rep(NA,Norig)
	  y = rep(NA,Norig)
	  visible = rep(NA,Norig)
	  x[which.notna] = x.nona
	  y[which.notna] = y.nona
	  visible[which.notna] = visible.nona
	  if (!is.null(rot.lon)) {
	    rot.lon.nona = rot.lon
	    rot.lat.nona = rot.lat
	    rot.lon = rep(NA,Norig)
	    rot.lat = rep(NA,Norig)
	    rot.lon[which.notna] = rot.lon.nona
	    rot.lat[which.notna] = rot.lat.nona
	  }
	}
	
	return(list(x=x,y=y,rot.lon=rot.lon,rot.lat=rot.lat,visible=visible))
	
}
