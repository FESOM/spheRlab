sl.lon1D.c2i <-
function (lon.c) {
	
	if (length(sl.dim(lon.c)) != 1) {stop("lon.c must be a vector")}
	
	Nlon = length(lon.c)
	lon.i = rep(NA,Nlon+1)
	for (i in 2:Nlon) {
		lon.i[i] = sl.p2p(lon.c[i-1],0,lon.c[i],0,.5)$lon
	}
	lon.i[1] = sl.p2p(lon.c[2],0,lon.c[1],0,1.5)$lon
	lon.i[Nlon+1] = sl.p2p(lon.c[Nlon-1],0,lon.c[Nlon],0,1.5)$lon
	if (sl.gc.dist(c(lon.i[1],lon.c[1]),c(0,0)) > sl.gc.dist(c(lon.i[Nlon+1],lon.c[1]),c(0,0))) {
		lon.i[Nlon+1] = lon.i[1]
	}
	
	return(lon.i)
	
}
