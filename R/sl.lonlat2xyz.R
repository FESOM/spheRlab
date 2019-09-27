sl.lonlat2xyz <-
function(lonlatvec=NULL, lon=NULL, lat=NULL) {
  
	if (!is.null(lonlatvec)) {
	  if (length(lonlatvec) != 2) {
		  stop("lonlatvec must be a lon-lat vector of length 2 (or NULL)")
	  }
	  lon = lonlatvec[1]
	  lat = lonlatvec[2]
	}
  
	lon = pi / 180 * lon
	lat = pi / 180 * lat
	
	x = cos(lat) * cos(lon)
	y = cos(lat) * sin(lon)
	z = sin(lat)
	
	if (!is.null(lonlatvec)) {
	  return(c(x,y,z))
	} else {
	  return(list(x=x,y=y,z=z))
	}
	
}
