sl.plot.lonlatgrid <-
function (plot.init.res,lon.0=0,lat.0=0,lon.distance=10,lat.distance=lon.distance,pole.hole=FALSE,precision=1,
          lty=1,lwd=.25,col="black",labels=FALSE,labels.lat.every=3,labels.lat.first=ceiling(labels.lat.every/2),
          labels.lat.offsetlatlon=c(0,0),labels.lon.every=labels.lat.every,labels.lon.first=ceiling(labels.lon.every/2),
          labels.lon.offsetlatlon=c(0,0),labels.col="grey",labels.cex=1) {
	
	lon.distance = abs(lon.distance)
	lat.distance = abs(lat.distance)
	
	lon.start = lon.0
	while (lon.start >= lon.distance - 180) {
		lon.start = lon.start - lon.distance
	}
	lons = seq(lon.start,180,lon.distance)
	Nlon = length(lons)
	if (lons[Nlon] == 180 && lons[1] == -180) {
		Nlon = Nlon - 1
		lons = lons[1:Nlon]
	}
	
	lat.start = lat.0
	while (lat.start > lat.distance - 90) {
		lat.start = lat.start - lat.distance
	}
	lats = seq(lat.start,90,lat.distance)
	Nlat = length(lats)
	if (lats[Nlat] == 90) {
		Nlat = Nlat - 1
		lats = lats[1:Nlat]
	}
	
	lines.lonlatgrid = list()
	lon = seq(-180,180,precision)
	if (lon[length(lon)] != 180) {lon = c(lon,180)}
	for (n in 1:Nlat) {
		lines.lonlatgrid[[n]] = list(lon=lon,lat=rep(lats[n],length(lon)))
	}
	
	if (pole.hole) {
		lat.minmax = c(lats[1],lats[Nlat])
	} else {
		lat.minmax = c(-90,90)
	}
	lat = seq(lat.minmax[1],lat.minmax[2],precision)
	if (lat[length(lat)] != lat.minmax[2]) {lat = c(lat,lat.minmax[2])}
	for (n in ((1:Nlon)+Nlat)) {
		lines.lonlatgrid[[n]] = list(lon=rep(lons[n-Nlat],length(lat)),lat=lat)
	}
	
	for (n in 1:(Nlat+Nlon)) {
		sl.plot.lines(plot.init.res,lon=lines.lonlatgrid[[n]]$lon,lat=lines.lonlatgrid[[n]]$lat,col=col,lwd=lwd,lty=lty)
	}
	
	if (labels) {
	  lons.medians = (lons[1:(Nlon-1)] + lons[2:Nlon]) / 2
	  lats.medians = (lats[1:(Nlat-1)] + lats[2:Nlat]) / 2
	  labels.lat.first.x = labels.lat.first - ceiling(labels.lat.every/2)
	  if (labels.lat.first.x < 1) {labels.lat.first.x = labels.lat.first.x + labels.lat.every}
	  labels.lon.first.x = labels.lon.first - ceiling(labels.lon.every/2)
	  if (labels.lon.first.x < 1) {labels.lon.first.x = labels.lon.first.x + labels.lon.every}
	  lat.lons = lons.medians[seq(labels.lon.first.x,Nlon-1,labels.lon.every)]
	  lat.lats = lats[seq(labels.lat.first,Nlat,labels.lat.every)]
	  lon.lons = lons[seq(labels.lon.first,Nlon,labels.lon.every)]
	  lon.lats = lats.medians[seq(labels.lat.first.x,Nlat-1,labels.lat.every)]
	  sl.plot.lonlatlabels(plot.init.res,lat.lons=lat.lons,lat.lats=lat.lats,lat.offsetlatlon=labels.lat.offsetlatlon,
	                       lon.lons=lon.lons,lon.lats=lon.lats,lon.offsetlatlon=labels.lon.offsetlatlon,
	                       col=labels.col,cex=labels.cex)
	}
	
}
