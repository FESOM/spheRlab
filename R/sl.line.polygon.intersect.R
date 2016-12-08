sl.line.polygon.intersect <-
function(line.lon,line.lat,polygon.lon,polygon.lat) {
	
	polygon.lon.ext = c(polygon.lon,polygon.lon[1])
	polygon.lat.ext = c(polygon.lat,polygon.lat[1])
	
	lines.intersect = NULL
	lon = NULL
	lat = NULL
	lon2 = NULL
	lat2 = NULL
	x = NULL
	y = NULL
	z = NULL
	x2 = NULL
	y2 = NULL
	z2 = NULL
	
	for (n in 1:length(polygon.lon)) {
		lli.res = sl.line.line.intersect(line.lon,line.lat,polygon.lon.ext[n:(n+1)],polygon.lat.ext[n:(n+1)])
		lines.intersect = c(lines.intersect,lli.res$lines.intersect)
		lon = c(lon,lli.res$lon)
		lat = c(lat,lli.res$lat)
		lon2 = c(lon2,lli.res$lon2)
		lat2 = c(lat2,lli.res$lat2)
		x = c(x,lli.res$x)
		y = c(y,lli.res$y)
		z = c(z,lli.res$z)
		x2 = c(x2,lli.res$x2)
		y2 = c(y2,lli.res$y2)
		z2 = c(z2,lli.res$z2)
	}
	anylines.intersect = as.logical(max(lines.intersect))
	
	return(list(anylines.intersect=anylines.intersect,lines.intersect=lines.intersect,lon=lon,lat=lat,lon2=lon2,lat2=lat2,x=x,y=y,z=z,x2=x2,y2=y2,z2=z2))
	
}
