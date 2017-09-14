sl.angle <-
function(lon,lat,return.degree=FALSE) {
	if (length(lon) != 3 || length(lat) != 3) {
		stop("lon and lat must be vectors of length 3!")
	}
	alpha = lon[2] + 90
	beta = 90 - lat[2]
	gamma = 0
	rot.res = sl.rot(lon,lat,alpha,beta,gamma,return.xyz=TRUE)
	x = rot.res$x
	y = rot.res$y
	a = c(x[2],y[2])
	b = c(x[1],y[1])
	c = c(x[3],y[3])
	ab = b - a
	ac = c - a
	options(warn=-1)
	ang = as.numeric(acos((ab/sqrt(sum(ab^2))) %*% (ac/sqrt(sum(ac^2)))))
	options(warn=1)
	if (return.degree) {ang=ang*360/2/pi}
	return(ang)
}
