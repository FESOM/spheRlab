sl.inpolygon.convex <-
function (lon.point,lat.point,lon,lat,check.ccw.convex=TRUE,border.as.in=TRUE) {

	N = length(lon)
	if (length(lat) != N) {stop("lon and lat must have same length")}
	res = TRUE
	reordered = FALSE
	
	repeat {
		if (!check.ccw.convex) {break}
		lon.ext = c(lon,lon[1:2])
		lat.ext = c(lat,lat[1:2])
		for (n in 1:N) {
			check.res = sl.checkposition(c(lon.ext[n],lat.ext[n]),c(lon.ext[n+1],lat.ext[n+1]),c(lon.ext[n+2],lat.ext[n+2]))
			if (check.res < 0) {
				if (!reordered) {
					warning("polygon either not convex or not ordered; trying reordered polygon")
					lon = lon[N:1]
					lat = lat[N:1]
					next
				} else {stop("polygon not convex")}
			}
		}
		if (check.res >= 0) {break}
	}
	
	lon.ext = c(lon,lon[1])
	lat.ext = c(lat,lat[1])
	for (n in 1:N) {
		check.res = sl.checkposition(c(lon.ext[n],lat.ext[n]),c(lon.ext[n+1],lat.ext[n+1]),c(lon.point,lat.point))
		if (check.res <= 0) {
			if (check.res == 0 && border.as.in) {next}
			res = FALSE
			break
		}
	}

	return(res)
	
}
