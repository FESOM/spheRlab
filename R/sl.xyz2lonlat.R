sl.xyz2lonlat <-
function(xyzvec) {
	if (length(xyzvec) != 3) {stop("xyzvec must be a cartesian vector of length 3!")}
	xyzvec = xyzvec / sl.cart.dist(xyzvec)
	rad = pi / 180
	lon = atan2(xyzvec[2],xyzvec[1]) / rad
	lat = asin(xyzvec[3]) / rad
	return(c(lon,lat))
}
