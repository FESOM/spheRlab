sl.p2p <-
function(lon1,lat1,lon2,lat2,frac) {
  if (anyNA(c(lon1,lat1,lon2,lat2,frac))) {return(list(lon=NA,lat=NA))}
  if (lon1==lon2) {if (lat1==lat2) {return(list(lon=lon1,lat=lat1))}}
	if (frac < 0) {
		lon1tmp = lon1
		lat1tmp = lat1
		lon1 = lon2
		lat1 = lat2
		lon2 = lon1tmp
		lat2 = lat1tmp
		frac = -frac
	}
	if (lon1 == lon2 && lat1 == lat2) {
		warning("points are identical")
		return(list(lon=lon1,lat=lat2))
	}
	alpha = lon1 + 90
	beta = 90 - lat1
	gamma = 0
	rot.res = sl.rot(lon2,lat2,alpha,beta,gamma)
	rot.lon = rot.res$lon
	pointdist = 90 - rot.res$lat
	if (pointdist == 180 && !is.integer(frac)) {stop("points are opposite, direction undefined")}
	movedist = pointdist * frac
	if (movedist > 180) {
		if (movedist > 360) {
			movedist = movedist %% 360
		}
		if (movedist > 180) {
			movedist = 360 - movedist
			rot.lon = rot.lon + 180
		}
	}
	rot.lat = 90 - movedist
	rot.res = sl.rot(rot.lon,rot.lat,alpha,beta,gamma,invert=TRUE)
	return(list(lon=rot.res$lon,lat=rot.res$lat))
}
