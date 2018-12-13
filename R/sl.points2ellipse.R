sl.points2ellipse <-
function (lon = NULL, lat = NULL, prob.within = 0.95, npoints = 360) {
	
  require(mixtools)
  
  baryc = sl.barycenter(lon = lon, lat = lat)
  abg = sl.lonlatrot2abg(c(baryc$lon,baryc$lat,0))
  rot.res = sl.rot(lon, lat, alpha=abg[1], beta=abg[2], gamma=abg[3], return.xyz = TRUE)
  mat = matrix(c(rot.res$x,rot.res$y),ncol=2)
  
  elli = ellipse(mu = colMeans(mat), sigma = cov(mat), alpha = 1-prob.within, npoints = npoints, draw = FALSE)
  elli.xysqsum = elli[,1]^2 + elli[,2]^2
  if (any(elli.xysqsum > 1)) {
    stop("ellipse is too wide for back-projection from xy-plane to sphere, try smaller 'prob.within'")
  }
  elli.z = sqrt(1 - (elli.xysqsum))
	elli.lonlatrot = apply(matrix(c(elli[,1],elli[,2],elli.z),ncol=3),1,sl.xyz2lonlat)
	elli.lonlat = sl.rot(lon=elli.lonlatrot[1,], lat=elli.lonlatrot[2,],
	                     alpha=abg[1], beta=abg[2], gamma=abg[3], invert=TRUE, return.xyz = FALSE)
	
	return(list(lon=elli.lonlat$lon, lat=elli.lonlat$lat))
	
}
