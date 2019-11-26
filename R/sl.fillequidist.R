sl.fillequidist <-
function(lon,lat,np=1,method="xyz",return.xyz=FALSE) {
  
  if (length(lat) > 2) {warning("more than 2 points provided, using only first two!")}
  
  if (method=="xyz") {
    
    rad = pi / 180
    lon = lon * rad
    lat = lat * rad
    x = cos(lat) * cos(lon)
    y = cos(lat) * sin(lon)
    z = sin(lat)
    fac2 = (1:np) / (np + 1)
    fac1 = 1 - fac2
    x.i = fac1*x[1] + fac2*x[2]
    y.i = fac1*y[1] + fac2*y[2]
    z.i = fac1*z[1] + fac2*z[2]
    dist = sqrt(x.i^2 + y.i^2 + z.i^2)
    x = x.i / dist
    y = y.i / dist
    z = z.i / dist
    lon = atan2(y,x) / rad
    lat = asin(z) / rad
    
  } else if (method == "gc") {
    
    alpha = lon[1] + 90
    beta = 90 - lat[1]
    gamma = 0
    rot.res = sl.rot(lon[2],lat[2],alpha,beta,gamma)
    rot.lon = rep(rot.res$lon, np)
    pointdist = 90 - rot.res$lat
    if (pointdist == 180) {stop("points are opposite, direction undefined")}
    movedist = pointdist / (np + 1)
    rot.lat = 90 - seq(movedist, np*movedist, movedist)
    rot.res = sl.rot(rot.lon,rot.lat,alpha,beta,gamma,invert=TRUE,return.xyz=return.xyz)
    lon = rot.res$lon
    lat = rot.res$lat
    if (return.xyz) {
      x = rot.res$x
      y = rot.res$y
      z = rot.res$z
    }
    
  } else {stop("'method' must be 'xyz' or 'gc'")}
  
  if (return.xyz) {
    return(list(lon=lon,lat=lat,x=x,y=y,z=z))
  } else {
    return(list(lon=lon,lat=lat))
  }
  
}
