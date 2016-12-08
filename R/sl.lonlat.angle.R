sl.lonlat.angle <-
function (lon1,lat1,lon2,lat2) {
	
	if (is.na(lon1) || is.na(lat1) || is.na(lon2) || is.na(lat2)) {return(NA)}
	if (max(abs(c(lat1,lat2))) > 90) {stop("latitude out of valid range")}
	
	alpha = lon1 + 90
	beta = 90 - lat1
	gamma = 0
	rot.res = sl.rot(lon2,lat2,alpha,beta,gamma)
	return(90-rot.res$lat)
	
}
