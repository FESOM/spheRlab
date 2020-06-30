sl.rot.vec <- function(al, be, ga, lon, lat, urot, vrot, flag){
  #   first get another coordinate
  if (flag==1){
    rlola = sl.rot(lon, lat, al, be, ga) # formerly (rlon,rlat)=scalar_g2r(al, be, ga, lon, lat)
    rlon = rlola$lon
    rlat = rlola$lat
  } else if (flag==0) {
    rlon=lon
    rlat=lat
    lola = sl.rot(rlon, rlat, al, be, ga, invert = T)  
    lon = lola$lon
    lat = lola$lat
  } else {
    stop("input parameter 'flag' must be either 0 or 1")
  }
  
  rotate_matrix = sl.assemble.eulermatrix(al, be, ga, do.d2r=TRUE)
  rotate_matrix=t(rotate_matrix)

  #   vector in rotated Cartesian
  gU = sl.uv2uvw(urot, vrot, rlon, rlat, F)
  txg = gU$u; tyg = gU$v; tzg = gU$w
   
  #   vector in geo Cartesian (matrix multiplication)
  txr=rotate_matrix[1,1]*txg + rotate_matrix[1,2]*tyg + rotate_matrix[1,3]*tzg 
  tyr=rotate_matrix[2,1]*txg + rotate_matrix[2,2]*tyg + rotate_matrix[2,3]*tzg 
  tzr=rotate_matrix[3,1]*txg + rotate_matrix[3,2]*tyg + rotate_matrix[3,3]*tzg 
  
  #   vector in geo coordinate
  rU = sl.uvw2uv(txr, tyr, tzr, lon, lat, F)
  u = rU$u; v = rU$v

  return(list(u=u,v=v))
}