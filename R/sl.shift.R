sl.shift <-
function(lon,lat,udt,vdt,Rsphere=1) {
  if (anyNA(c(lon,lat,udt,vdt,Rsphere))) {return(list(lon=NA,lat=NA))}
  
  lon.rad = lon*2*pi/360
  lat.rad = lat*2*pi/360
  
  u = udt / Rsphere
  v = vdt / Rsphere
  len = sqrt(u^2+v^2)
  if (len >= pi/2) {stop("sl.shift() works only for shifts by less than 90 degree")}
  
  dxyz.unitvec.u = c(-sin(lon.rad), cos(lon.rad), 0)
  dxyz.unitvec.v = c(-cos(lon.rad)*sin(lat.rad), -sin(lon.rad)*sin(lat.rad), cos(lat.rad))
  
  lonlat = sl.xyz2lonlat(sl.lonlat2xyz(c(lon,lat)) + (u*dxyz.unitvec.u + v*dxyz.unitvec.v) * tan(len)/len)
  
	return(list(lon=lonlat[1],lat=lonlat[2]))
}
