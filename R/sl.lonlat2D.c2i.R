sl.lonlat2D.c2i <-
function (lon.c,lat.c,extrapol.factor=0.5) {
	
	if (length(sl.dim(lon.c)) != 2) {stop("lon.c must be a matrix")}
  if (!identical(sl.dim(lon.c),sl.dim(lat.c))) {stop("lon.c and lat.c must be matrices of the same size")}
	
  Nrow = nrow(lon.c)
  Ncol = ncol(lon.c)
  
  lon.i = matrix(nrow = Nrow + 1, ncol = Ncol + 1)
  lat.i = matrix(nrow = Nrow + 1, ncol = Ncol + 1)
  
	for (ro in 2:Nrow) {
	  for (co in 2:Ncol) {
	    pnt.diag1 = sl.p2p(lon1=lon.c[ro-1,co-1], lat1=lat.c[ro-1,co-1], lon2=lon.c[ro,co], lat2=lat.c[ro,co], frac=0.5)
	    pnt.diag2 = sl.p2p(lon1=lon.c[ro-1,co], lat1=lat.c[ro-1,co], lon2=lon.c[ro,co-1], lat2=lat.c[ro,co-1], frac=0.5)
	    pnt = sl.p2p(lon1=pnt.diag1$lon, lat1=pnt.diag1$lat, lon2=pnt.diag2$lon, lat2=pnt.diag2$lat, frac=0.5)
	    lon.i[ro,co] = pnt$lon
	    lat.i[ro,co] = pnt$lat
	  }
	}
  ro = 1
  for (co in 2:Ncol) {
    pnt.col1 = sl.p2p(lon1=lon.c[ro+1,co-1], lat1=lat.c[ro+1,co-1], lon2=lon.c[ro,co-1], lat2=lat.c[ro,co-1], frac=extrapol.factor+1)
    pnt.col2 = sl.p2p(lon1=lon.c[ro+1,co], lat1=lat.c[ro+1,co], lon2=lon.c[ro,co], lat2=lat.c[ro,co], frac=extrapol.factor+1)
    pnt = sl.p2p(lon1=pnt.col1$lon, lat1=pnt.col1$lat, lon2=pnt.col2$lon, lat2=pnt.col2$lat, frac=0.5)
    lon.i[ro,co] = pnt$lon
    lat.i[ro,co] = pnt$lat
  }
  ro = Nrow
  for (co in 2:Ncol) {
    pnt.col1 = sl.p2p(lon1=lon.c[ro-1,co-1], lat1=lat.c[ro-1,co-1], lon2=lon.c[ro,co-1], lat2=lat.c[ro,co-1], frac=extrapol.factor+1)
    pnt.col2 = sl.p2p(lon1=lon.c[ro-1,co], lat1=lat.c[ro-1,co], lon2=lon.c[ro,co], lat2=lat.c[ro,co], frac=extrapol.factor+1)
    pnt = sl.p2p(lon1=pnt.col1$lon, lat1=pnt.col1$lat, lon2=pnt.col2$lon, lat2=pnt.col2$lat, frac=0.5)
    lon.i[ro+1,co] = pnt$lon
    lat.i[ro+1,co] = pnt$lat
  }
  co = 1
  for (ro in 2:Nrow) {
    pnt.row1 = sl.p2p(lon1=lon.c[ro-1,co+1], lat1=lat.c[ro-1,co+1], lon2=lon.c[ro-1,co], lat2=lat.c[ro-1,co], frac=extrapol.factor+1)
    pnt.row2 = sl.p2p(lon1=lon.c[ro,co+1], lat1=lat.c[ro,co+1], lon2=lon.c[ro,co], lat2=lat.c[ro,co], frac=extrapol.factor+1)
    pnt = sl.p2p(lon1=pnt.row1$lon, lat1=pnt.row1$lat, lon2=pnt.row2$lon, lat2=pnt.row2$lat, frac=0.5)
    lon.i[ro,co] = pnt$lon
    lat.i[ro,co] = pnt$lat
  }
  co = Ncol
  for (ro in 2:Nrow) {
    pnt.row1 = sl.p2p(lon1=lon.c[ro-1,co-1], lat1=lat.c[ro-1,co-1], lon2=lon.c[ro-1,co], lat2=lat.c[ro-1,co], frac=extrapol.factor+1)
    pnt.row2 = sl.p2p(lon1=lon.c[ro,co-1], lat1=lat.c[ro,co-1], lon2=lon.c[ro,co], lat2=lat.c[ro,co], frac=extrapol.factor+1)
    pnt = sl.p2p(lon1=pnt.row1$lon, lat1=pnt.row1$lat, lon2=pnt.row2$lon, lat2=pnt.row2$lat, frac=0.5)
    lon.i[ro,co+1] = pnt$lon
    lat.i[ro,co+1] = pnt$lat
  }
  ro = 1
  co = 1
  pnt = sl.p2p(lon1=lon.c[ro+1,co+1], lat1=lat.c[ro+1,co+1], lon2=lon.c[ro,co], lat2=lat.c[ro,co], frac=extrapol.factor+1)
  lon.i[ro,co] = pnt$lon
  lat.i[ro,co] = pnt$lat
  ro = 1
  co = Ncol
  pnt = sl.p2p(lon1=lon.c[ro+1,co-1], lat1=lat.c[ro+1,co-1], lon2=lon.c[ro,co], lat2=lat.c[ro,co], frac=extrapol.factor+1)
  lon.i[ro,co+1] = pnt$lon
  lat.i[ro,co+1] = pnt$lat
  ro = Nrow
  co = 1
  pnt = sl.p2p(lon1=lon.c[ro-1,co+1], lat1=lat.c[ro-1,co+1], lon2=lon.c[ro,co], lat2=lat.c[ro,co], frac=extrapol.factor+1)
  lon.i[ro+1,co] = pnt$lon
  lat.i[ro+1,co] = pnt$lat
  ro = Nrow
  co = Ncol
  pnt = sl.p2p(lon1=lon.c[ro-1,co-1], lat1=lat.c[ro-1,co-1], lon2=lon.c[ro,co], lat2=lat.c[ro,co], frac=extrapol.factor+1)
  lon.i[ro+1,co+1] = pnt$lon
  lat.i[ro+1,co+1] = pnt$lat
	
	return(list(lon.i = lon.i, lat.i = lat.i))
	
}
