sl.fillequidist <-
function(lon,lat,np=1) {
	# warning: I realized that this method is imprecise for large distances
	#          -> reformulate: rotate 1 point to pole - lat-equidist - rotate back
	if (length(lat) > 2) {warning("more than 2 points provided, using only first two!")}
	rad = pi / 180
	lon = lon * rad
	lat = lat * rad
	x = cos(lat) * cos(lon)
	y = cos(lat) * sin(lon)
	z = sin(lat)
	fac2 = (1:np) / (np + 1)
	fac1 = 1 - fac2
	x.i = fac1*x[1] + fac2*x[2]
	y.i = fac1*y[1] + fac2*y[2]
	z.i = fac1*z[1] + fac2*z[2]
	dist = sqrt(x.i^2 + y.i^2 + z.i^2)
	x.i = x.i / dist
	y.i = y.i / dist
	z.i = z.i / dist
	lon = atan2(y.i,x.i) / rad
	lat = asin(z.i) / rad
	return(list(lon=lon,lat=lat))
}
