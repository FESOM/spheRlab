sl.circle <-
function (lon, lat, radius, resolution = 1, repeat.first = TRUE) {
  
  if (sl.dim(lon) != 1 || sl.dim(lat) != 1 || sl.dim(radius) != 1) {stop("all arguments must be single scalars")}
  
  lats = 90-radius
  lons = seq(0,360,resolution)
  N = length(lons)
  if (lons[N] == 360) {lons = lons[1:N]}
  
  abg = sl.lonlatrot2abg(c(lon,lat,0))
  lonlat = sl.rot(lon = lons, lat = lats, alpha = abg[1], beta = abg[2], gamma = abg[3],return.xyz = FALSE, invert = TRUE)
  lons = lonlat$lon
  lats = lonlat$lat
  
  if (repeat.first) {
    lons = c(lons,lons[1])
    lats = c(lats,lats[1])
  }
  
  return(list(lon=lons,lat=lats))
	
}
