sl.uvw2uv <- function(u, v, w, lon, lat, do.d2r = T){
  if(do.d2r){d2r = pi/180} else {d2r = 1} # degree 2 radian conversion or not
  if(length(u)==length(v) & length(lat) == length(lon) & length(lat)==length(u) & length(lat)==length(w)){
    rlon = lon*d2r; rlat = lat*d2r
    U=-sin(rlon)*u + cos(rlon)*v
    V=-sin(rlat)*cos(rlon)*u - sin(rlat)*sin(rlon)*v + cos(rlat)*w
    return(list(u=U, v=V))
  } else {
    warning("Lengths of input vectors do not match, returning NULL.")
    return(NULL)
  }
}