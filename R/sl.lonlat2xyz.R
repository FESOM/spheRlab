sl.lonlat2xyz <-
function(lonlatvec) {
	if (length(lonlatvec) != 2) {
		stop("lonlatvec must be a lon-lat vector of length 2!")
	}
	rad = pi / 180
	lonlatvec = lonlatvec * rad
	lon = lonlatvec[1]
	lat = lonlatvec[2]
	x = cos(lat) * cos(lon)
	y = cos(lat) * sin(lon)
	z = sin(lat)
	return(c(x,y,z))
}
