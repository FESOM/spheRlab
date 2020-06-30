sl.uv2uvw <- function(u, v, lon, lat, do.d2r = T){
  if(do.d2r){d2r = pi/180} else {d2r = 1} # degree 2 radian conversion or not
  if(length(u)==length(v) & length(lat) == length(lon) & length(lat)==length(u)){
    rlon    = lon*d2r; rlat    = lat*d2r
    u_ = -u*sin(rlon) - v*sin(rlat)*cos(rlon)
    v_ = +u*cos(rlon) - v*sin(rlat)*sin(rlon)
    w_ =                v*cos(rlat)
    return(list(u=u_,v=v_,w=w_))
  } else {
    warning("Lengths of input vectors do not match, returning NULL.")
    return(NULL)
  }
}