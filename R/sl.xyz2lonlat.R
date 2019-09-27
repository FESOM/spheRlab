sl.xyz2lonlat <-
function(xyzvec=NULL, x=NULL, y=NULL, z=NULL) {
  
	if (!is.null(xyzvec)) {
	  if (length(xyzvec) != 3) {stop("xyzvec must be a cartesian vector of length 3 (or NULL)")}
	  xyzvec = xyzvec / sqrt(sum(xyzvec^2))
	  x = xyzvec[1]
	  y = xyzvec[2]
	  z = xyzvec[3]
	} else {
	  len = sqrt(x^2 + y^2 + z^2)
	  x = x / len
	  y = y / len
	  z = z / len
	}
  
	rad = pi / 180
	lon = atan2(y,x) / rad
	lat = asin(z) / rad
	
	if (!is.null(xyzvec)) {
	  return(c(lon,lat))
	} else {
	  return(list(lon=lon,lat=lat))
	}
	
}
