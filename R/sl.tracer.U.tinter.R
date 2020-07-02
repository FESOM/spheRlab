sl.tracer.U.tinter <- function(U_p, t_p, t, mode = "interpolation"){
  Nt = length(t_p)
  
  if (any(diff(t_p)<0)){stop("time axis 't_p' must have strictly increasing values")}
  if ((t < t_p[1]) | (t > t_p[Nt])){stop("'t' must be in the range of 't_p'")}
  
  # find neighboring values of 't' on time grid 't_p'
  it0 = max(which(t_p<=t)); it1 = min(which(t_p>=t))
  t0 = t_p[it0] ; t1 = t_p[it1]
  if (length(it0)==0 | length(it1)==0){stop(".. something is fishy (time axis)")}
  
  if (mode == "interpolation"){
    # note that this interprets the t_grid as the time at which the value for U is valid!
    if(length(dim(U_p))==2){
      if(it0==it1){return(U_p[,it0])}
      y0 = U_p[,it0]
      y1 = U_p[,it1]
    } else {
      if(it0==it1){return(U_p[,,it0])}
      y0 = U_p[,,it0]
      y1 = U_p[,,it1]
    }
    return(y0 + (y1-y0)/(t1-t0)*(t-t0))
  } else if (mode == "stepfunction"){
    # note that the time grid is interpreted as vector of the left boundary of a time interval
    # over which the velocity remains constant
    if(length(dim(U_p))==2){return(U_p[,it0])} else {return(U_p[,,it0])}
  } else {warning("'mode' must either be 'interpolation' or 'stepfunction'")}
}
