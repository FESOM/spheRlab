sl.plot.lonlatgrid <-
function (plot.init.res,lon.0=0,lat.0=0,lon.distance=NULL,lat.distance=NULL,lon.range=c(-180,180),lat.range=c(-90,90),autodensity.f=1,pole.hole=FALSE,precision=1,
          lty=1,lwd=.25,col="black",labels=FALSE,labels.lat.every=3,labels.lat.first=ceiling(labels.lat.every/2),
          labels.lat.offsetlatlon=c(0,0),labels.lon.every=labels.lat.every,labels.lon.first=ceiling(labels.lon.every/2),
          labels.lon.offsetlatlon=c(0,0),labels.col="grey",labels.round.digits=NULL,labels.cex=1) {
  
  if (is.null(lat.distance)) {
    if (plot.init.res$projection == "polar") {
      radius = 90 - plot.init.res$polar.latbound
    } else if (plot.init.res$projection == "regpoly") {
      radius = 90 - plot.init.res$regpoly.lat0
    } else if (plot.init.res$projection == "lonlat") {
      radius = abs(diff(plot.init.res$lonlat.latrange)/2)
    } else if (plot.init.res$projection == "platon") {
      radius = 90 - plot.init.res[[1]]$regpoly.lat0
    } else if (plot.init.res$projection == "3D") {
      radius = 90 - plot.init.res[[1]]$polar.latbound
    }
    lat.distance = 2^(round(log2(radius / 4 / autodensity.f)))
    if (lat.distance > 1) {
      latlist = c(1.5,2,2.5,3,4.5,5,6,7.5,10,15,18,22.5,45)
      lat.distance = latlist[which.min(abs(log(latlist)-log(lat.distance)))]
    }
  }
  if (is.null(lon.distance)) {
    if (plot.init.res$projection == "polar") {
      radius = 90 - plot.init.res$polar.latbound
      latref = abs(plot.init.res$polar.lonlatrot[2])
    } else if (plot.init.res$projection == "regpoly") {
      radius = 90 - plot.init.res$regpoly.lat0
      latref = abs(plot.init.res$regpoly.lonlatrot[2])
    } else if (plot.init.res$projection == "lonlat") {
      radius = abs(diff(plot.init.res$lonlat.latrange)/2)
      latref = 0
    } else if (plot.init.res$projection == "platon") {
      radius = 90 - plot.init.res[[1]]$regpoly.lat0
      latref = 45
    } else if (plot.init.res$projection == "3D") {
      radius = 90 - plot.init.res[[1]]$polar.latbound
      latref = abs(plot.init.res[[1]]$polar.lonlatrot[2])
    }
    if (latref + radius > 90) {latref = abs(latref - ((latref+radius-90) / 2))}
    lon.distance = 2^(round(log2(radius / 4 / autodensity.f / cos(2*pi*latref/360))))
    if (lon.distance > 1) {
      lonlist = c(1.5,2,2.5,3,4,5,6,7.5,8,10,12,15,20,24,30,40,45,60,90)
      lon.distance = lonlist[which.min(abs(log(lonlist)-log(lon.distance)))]
    }
  }
	lon.distance = abs(lon.distance)
	lat.distance = abs(lat.distance)
	
	lat.range[1] = max(lat.range[1], -90)
	lat.range[2] = min(lat.range[2], 90)
	
	lon.start = lon.0
	while (lon.start < lon.range[1]) {
	  lon.start = lon.start + lon.distance
	}
	while (lon.start >= lon.distance + lon.range[1]) {
		lon.start = lon.start - lon.distance
	}
	if (lon.start > lon.range[2]) {
	  lons = NULL
	  Nlon = 0
	} else {
	  lons = seq(lon.start,lon.range[2],lon.distance)
	  Nlon = length(lons)
	  if (lons[Nlon] == 180 && lons[1] == -180) {
		  Nlon = Nlon - 1
		  lons = lons[1:Nlon]
	  }
	}
	
	lat.start = lat.0
	while (lat.start < lat.range[1]) {
	  lat.start = lat.start + lat.distance
	}
	while (lat.start >= lat.distance + lat.range[1]) {
		lat.start = lat.start - lat.distance
	}
	if (lat.start > lat.range[2]) {
	  Nlat = 0
	} else {
	  lats = seq(lat.start,lat.range[2],lat.distance)
	  Nlat = length(lats)
	  if (lats[Nlat] == 90) {
		  Nlat = Nlat - 1
		  lats = lats[1:Nlat]
	  }
	  if (lats[1] == -90) {
	    lats = lats[2:Nlat]
	    Nlat = Nlat - 1
	  }
	}
	
	if (Nlat+Nlon == 0) {
	  warning("no lines to be drawn by sl.plot.lonlatgrid")
	  return(NULL)
	}
	
	lines.lonlatgrid = list()
	
	if (Nlat > 0) {
	  lon = seq(lon.range[1],lon.range[2],precision)
	  if (lon[length(lon)] != lon.range[2]) {lon = c(lon,lon.range[2])}
	  for (n in 1:Nlat) {
		  lines.lonlatgrid[[n]] = list(lon=lon,lat=rep(lats[n],length(lon)))
	  }
	}
	
	if (Nlon > 0) {
	  lat.minmax = lat.range
	  if (pole.hole) {
	    if (lats[1] - lat.distance <= -90) {
		    lat.minmax[1] = lats[1]
	    }
	    if (lats[Nlat] + lat.distance >= 90) {
	      lat.minmax[2] = lats[Nlat]
	    }
	  }
	  lat = seq(lat.minmax[1],lat.minmax[2],precision)
	  if (lat[length(lat)] != lat.minmax[2]) {lat = c(lat,lat.minmax[2])}
	  for (n in ((1:Nlon)+Nlat)) {
		  lines.lonlatgrid[[n]] = list(lon=rep(lons[n-Nlat],length(lat)),lat=lat)
	  }
	}
	
	for (n in 1:(Nlat+Nlon)) {
		sl.plot.lines(plot.init.res,lon=lines.lonlatgrid[[n]]$lon,lat=lines.lonlatgrid[[n]]$lat,col=col,lwd=lwd,lty=lty)
	}
	
	if (labels) {
	  if (Nlon == 0 || Nlat == 0) {
	    warning("labels not possible (no longitudes or no latitudes are drawn)")
	    return(NULL)
	  }
	  lons.medians = c()
	  for (i in 1:Nlon) {
	    lons.medians = c(lons.medians, sl.p2p(lons[i],0,lons[i%%Nlon + 1],0,frac=0.5)$lon)
	  }
	  lats.medians = (lats[1:(Nlat-1)] + lats[2:Nlat]) / 2
	  labels.lat.first.x = labels.lat.first - ceiling(labels.lat.every/2)
	  if (labels.lat.first.x < 1) {labels.lat.first.x = labels.lat.first.x + labels.lat.every}
	  labels.lon.first.x = labels.lon.first - ceiling(labels.lon.every/2)
	  if (labels.lon.first.x < 1) {labels.lon.first.x = labels.lon.first.x + labels.lon.every}
	  if (Nlon < labels.lon.first.x || Nlat < labels.lat.first) {
	    lat.lons = NULL
	    lat.lats = NULL
	  } else {
	    lat.lons = lons.medians[seq(labels.lon.first.x,Nlon,labels.lon.every)]
	    lat.lats = lats[seq(labels.lat.first,Nlat,labels.lat.every)]
	  }
	  if (Nlon < labels.lon.first || (Nlat-1) < labels.lat.first.x) {
	    lon.lons = NULL
	    lon.lats = NULL
	  } else {
	    lon.lons = lons[seq(labels.lon.first,Nlon,labels.lon.every)]
	    lon.lats = lats.medians[seq(labels.lat.first.x,Nlat-1,labels.lat.every)]
	  }
	  sl.plot.lonlatlabels(plot.init.res,lat.lons=lat.lons,lat.lats=lat.lats,lat.offsetlatlon=labels.lat.offsetlatlon,
	                       lon.lons=lon.lons,lon.lats=lon.lats,lon.offsetlatlon=labels.lon.offsetlatlon,
	                       col=labels.col,labels.round.digits=labels.round.digits,cex=labels.cex)
	}
	
}
