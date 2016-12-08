sl.rot <-
function (lon,lat,alpha,beta,gamma,return.xyz=FALSE,invert=FALSE) {
	
	rad = pi / 180
	a = alpha * rad
	b = beta * rad
	c = gamma * rad
	lon = lon * rad
	lat = lat * rad
	
	rotmat = matrix(nrow=3,ncol=3)
	rotmat[1,1] = cos(c)*cos(a) - sin(c)*cos(b)*sin(a)
	rotmat[1,2] = cos(c)*sin(a) + sin(c)*cos(b)*cos(a)
	rotmat[1,3] = sin(c)*sin(b)
	rotmat[2,1] = -sin(c)*cos(a) - cos(c)*cos(b)*sin(a)
	rotmat[2,2] = -sin(c)*sin(a) + cos(c)*cos(b)*cos(a)
	rotmat[2,3] = cos(c)*sin(b)
	rotmat[3,1] = sin(b)*sin(a)
	rotmat[3,2] = -sin(b)*cos(a)
	rotmat[3,3] = cos(b)
	if (invert) {
		rotmat = solve(rotmat)
	}
	
	x = cos(lat) * cos(lon)
	y = cos(lat) * sin(lon)
	z = sin(lat)
	
	x.rot = rotmat[1,1]*x + rotmat[1,2]*y + rotmat[1,3]*z
	y.rot = rotmat[2,1]*x + rotmat[2,2]*y + rotmat[2,3]*z
	z.rot = rotmat[3,1]*x + rotmat[3,2]*y + rotmat[3,3]*z
	z.rot[z.rot>1] = 1
	z.rot[z.rot<(-1)] = -1
	
	lon.rot = atan2(y.rot,x.rot) / rad
	options(warn=-1)
	lat.rot = asin(z.rot) / rad
	options(warn=1)
	
	if (return.xyz) {
		return(list(lon=lon.rot,lat=lat.rot,x=x.rot,y=y.rot,z=z.rot))
	} else {
		return(list(lon=lon.rot,lat=lat.rot))
	}
}
