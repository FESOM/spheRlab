sl.boundingcircle <-
function(lon=NULL, lat=NULL, method="ritter", verbose=TRUE) {
  
  if (method == "ritter") {
    if (verbose) {print("Note that the bounding circle returned with method 'ritter' is not the smallest possible circumcircle, but typically 5-20% larger than that.")}
  } else {
    stop("No method other than 'ritter' implemented so far")
  }
  
  if (is.null(lon) || is.null(lat)) {stop("both lon and lat must be provided")}
  if (!identical(sl.dim(lon), sl.dim(lat))) {stop("lon and lat must have the same dimensions")}
  
  not.na = (!is.na(lon) & !is.na(lat))
  if (sum(not.na) == 0) {stop("lon and lat must have at least one common element that is not 'NA'")}
  lon = lon[not.na]
  lat = lat[not.na]
  
  dim.in = sl.dim(lon)
  lon = as.vector(lon)
  lat = as.vector(lat)
  N = length(lon)
  
  clon = lon[1]
  clat = lat[1]
  radius = 0
  
  if (N>1) {
    lon = lon[2:N]
    lat =lat[2:N]
  }
  
  rad = 360 / 2 / pi
  
  repeat {
    
    if (length(lon) == 0) {break}
    
    dst = sl.gc.dist(c(clon,lon),c(clat,lat),sequential=FALSE) * rad
    maxdst = max(dst)
    
    if (maxdst <= radius) {break}
    
    maxind = match(maxdst,dst)
    
    keep = (dst > radius)
    keep[maxind] = FALSE
    
    frac = (1 - radius/maxdst) / 2
    radius = radius + (maxdst - radius) / 2
    cnew = sl.p2p(lon1=clon,lat1=clat,lon2=lon[maxind],lat2=lat[maxind],frac=frac)
    clon = cnew$lon
    clat = cnew$lat
    
    lon = lon[keep]
    lat = lat[keep]
    
  }
	
	return(list(center_lon=clon,center_lat=clat,radius=radius))
	
}
