sl.triag.area <-
function(lon,lat) {
	if (length(lon) != 3 || length(lat) != 3) {
		stop("lon and lat must be vectors of length 3!")
	}
	ang = numeric(3)
	for (i in 1:3) {
		alpha = lon[i] + 90
		beta = 90 - lat[i]
		gamma = 0
		rot.res = sl.rot(lon,lat,alpha,beta,gamma,return.xyz=TRUE)
		x = rot.res$x
		y = rot.res$y
		a = c(x[i],y[i])
		b = c(x[i%%3+1],y[i%%3+1])
		c = c(x[(i+1)%%3+1],y[(i+1)%%3+1])
		ab = b - a
		ac = c - a
		options(warn=-1)
		ang[i] = acos((ab/sqrt(sum(ab^2))) %*% (ac/sqrt(sum(ac^2))))
		options(warn=1)
	}
	if (sum(is.na(ang)) > 0) {
		return(0)
	} else {
		return(sum(ang)-pi)
	}
}
