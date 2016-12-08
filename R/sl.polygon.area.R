sl.polygon.area <-
function(stamp.lon,stamp.lat,ref.lon=NA,ref.lat=NA,small=FALSE,absval=TRUE) {
	N = length(stamp.lon)
	if (is.na(ref.lon)) {
		ref = sl.barycenter(lon=stamp.lon,lat=stamp.lat)
		ref.lon = ref$lon
		ref.lat = ref$lat
	}
	triag.areas = numeric(N)
	for (i in 1:N) {
		lons = c(ref.lon,stamp.lon[i],stamp.lon[i%%N+1])
		lats = c(ref.lat,stamp.lat[i],stamp.lat[i%%N+1])
		checkpos = sl.checkposition(c(lons[1],lats[1]),c(lons[2],lats[2]),c(lons[3],lats[3]))
		if (small) {
			triag.areas[i] = sl.smalltriag.area(lons,lats) * checkpos
		} else {
			triag.areas[i] = sl.triag.area(lons,lats) * checkpos
		}
	}
	if (absval) {
		return(abs(sum(triag.areas)))
	} else {
		return(sum(triag.areas))
	}
}
