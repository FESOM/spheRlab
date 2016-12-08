sl.centroid <-
function(stamp.lon,stamp.lat,ref.lon=NA,ref.lat=NA) {
	N = length(stamp.lon)
	if (is.na(ref.lon)) {
		ref = sl.barycenter(lon=stamp.lon,lat=stamp.lat)
		ref.lon = ref$lon
		ref.lat = ref$lat
	}
	triag.areaweight = numeric(N)
	triag.lon = numeric(N)
	triag.lat = numeric(N)
	for (i in 1:N) {
		lons = c(ref.lon,stamp.lon[i],stamp.lon[i%%N+1])
		lats = c(ref.lat,stamp.lat[i],stamp.lat[i%%N+1])
		checkpos = sl.checkposition(c(lons[1],lats[1]),c(lons[2],lats[2]),c(lons[3],lats[3]))
		triag.areaweight[i] = sl.triag.area(lons,lats) * checkpos
		barycenter.res = sl.barycenter(lon=lons,lat=lats)
		triag.lon[i] = barycenter.res$lon
		triag.lat[i] = barycenter.res$lat
	}
	centroid.res = sl.barycenter(lon=triag.lon,lat=triag.lat,weights=triag.areaweight)
	zeroweights = sum(triag.areaweight == 0)
	negweights = sum(triag.areaweight < 0)
	return(list(lon=centroid.res$lon,lat=centroid.res$lat,zeroweights=zeroweights,negweights=negweights))
}
