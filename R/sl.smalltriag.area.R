sl.smalltriag.area <-
function(lon,lat) {
	if (length(lon) != 3 || length(lat) != 3) {
		stop("lon and lat must be vectors of length 3!")
	}
	baryc.res = sl.barycenter(lon=lon,lat=lat)
	alpha = baryc.res$lon + 90
	beta = 90 - baryc.res$lat
	gamma = 0
	rot.res = sl.rot(lon,lat,alpha,beta,gamma,return.xyz=TRUE)
	x = rot.res$x
	y = rot.res$y
	a = c(x[1],y[1])
	b = c(x[2],y[2])
	c = c(x[3],y[3])
	ab = b - a
	ac = c - a
	return(abs(ab[1]*ac[2] - ab[2]*ac[1])/2)
}
