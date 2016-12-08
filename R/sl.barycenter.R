sl.barycenter <-
function(x=NA,y=NA,z=NA,lon=NA,lat=NA,weights=NA) {
	rad = pi / 180
	if (is.na(x[1])) {
		lon = lon * rad
		lat = lat * rad
		x = cos(lat) * cos(lon)
		y = cos(lat) * sin(lon)
		z = sin(lat)
	}
	if (is.na(weights[1])) {
		x.m = mean(x)
		y.m = mean(y)
		z.m = mean(z)
	} else {
		weights = weights / sum(weights)
		x.m = sum(x*weights)
		y.m = sum(y*weights)
		z.m = sum(z*weights)
	}
	dist = sqrt(x.m^2 + y.m^2 + z.m^2)
	x.m = x.m / dist
	y.m = y.m / dist
	z.m = z.m / dist
	lon = atan2(y.m,x.m) / rad
	lat = asin(z.m) / rad
	return(list(lon=lon,lat=lat))
}
