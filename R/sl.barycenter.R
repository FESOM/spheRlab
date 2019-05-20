sl.barycenter <-
function(x=NULL,y=NULL,z=NULL,lon=NULL,lat=NULL,weights=NULL,rm.na=TRUE) {
	rad = pi / 180
	if (is.null(x)) {
	  if (is.null(lon) || is.null(lat)) {stop("Please provide x,y,z or lon,lat")}
	  if (rm.na) {
	    if (is.null(weights) && any(is.na(c(lon,lat)))) {
	      inds = which(!is.na(lon) & !is.na(lat))
	      lon = lon[inds]
	      lat = lat[inds]
	    } else if (any(is.na(c(lon,lat,weights)))) {
	      inds = which(!is.na(lon) & !is.na(lat) & !is.na(weights))
	      lon = lon[inds]
	      lat = lat[inds]
	      weights = weights[inds]
	    }
	  }
		lon = lon * rad
		lat = lat * rad
		x = cos(lat) * cos(lon)
		y = cos(lat) * sin(lon)
		z = sin(lat)
	} else {
	  if (is.null(y) || is.null(z)) {stop("Please provide x,y,z or lon,lat")}
	  if (rm.na) {
	    if (is.null(weights) && any(is.na(c(x,y,z)))) {
	      inds = which(!is.na(x) & !is.na(y) & !is.na(z))
	      x = x[inds]
	      y = y[inds]
	      z = z[inds]
	    } else if (any(is.na(c(x,y,z,weights)))) {
	      inds = which(!is.na(x) & !is.na(y) & !is.na(z) & !is.na(weights))
	      x = x[inds]
	      y = y[inds]
	      z = z[inds]
	      weights = weights[inds]
	    }
	  }
	}
	if (is.null(weights)) {
		x.m = mean(x)
		y.m = mean(y)
		z.m = mean(z)
	} else {
		weights = weights / sum(weights)
		x.m = sum(x*weights)
		y.m = sum(y*weights)
		z.m = sum(z*weights)
	}
	dist = sqrt(x.m^2 + y.m^2 + z.m^2)
	x.m = x.m / dist
	y.m = y.m / dist
	z.m = z.m / dist
	lon = atan2(y.m,x.m) / rad
	lat = asin(z.m) / rad
	return(list(lon=lon,lat=lat))
}
