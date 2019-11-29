sl.angle <-
function(lon,lat,return.degree=FALSE,left=FALSE) {
  
	if (length(lon) != 3 || length(lat) != 3) {
		stop("lon and lat must be vectors of length 3!")
	}
	alpha = lon[2] + 90
	beta = 90 - lat[2]
	gamma = 0
	
	rot.res = sl.rot(lon,lat,alpha,beta,gamma,return.xyz=FALSE)
	ang = rot.res$lon[1] - rot.res$lon[3]
	if (ang < 0) {ang = ang + 360}
	if (ang >= 360) {ang = ang - 360}
	if (!left && ang > 180) {ang = 360 - ang}
	if (!return.degree) {ang = ang/360*2*pi}
	
	return(ang)
	
}
