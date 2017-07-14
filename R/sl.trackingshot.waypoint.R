sl.trackingshot.waypoint <-
  function(projection, time = 0, lonlat.lonrange=NULL,lonlat.latrange=NULL,polar.lonlatrot=NULL,polar.latbound=NULL,regpoly.lonlatrot=NULL,regpoly.N=NULL,regpoly.lat0=NULL,regpoly.rotfrac=NULL) {
    
    if(!is.character(projection)) stop("projection has to be a string")
    if(!is.numeric(time)) stop("time has to be numeric")
    if(length(time) > 1) stop("time has to be a numeric vector of length 1")
    if(time < 0) stop("time has to be a positiv number")
    
    lonlat.full = !is.null(lonlat.lonrange) && !is.null(lonlat.latrange)
    polar.full = !is.null(polar.lonlatrot) && !is.null(polar.latbound)
    regpoly.full = !is.null(regpoly.lonlatrot) && !is.null(regpoly.N) && !is.null(regpoly.lat0) && !is.null(regpoly.rotfrac)
    
    ret = list("projection" = projection, time = time);
    class(ret) <- "sl.waypoint"
    
    if(projection == "lonlat" && lonlat.full){
      if(!is.numeric(lonlat.lonrange)) stop("lonlat.lonrange has to be a numeric vector")
      if(!is.numeric(lonlat.latrange)) stop("lonlat.latrange has to be a numeric vector")
      if(length(lonlat.lonrange) != 2) stop("length of lonlat.lonrange has to be 2")
      if(length(lonlat.latrange) != 2) stop("length of lonlat.latrange has to be 2")
      
      ret$lonlat.lonrange = lonlat.lonrange
      ret$lonlat.latrange = lonlat.latrange
    }
    else if(projection == "polar" && polar.full){
      stop("polar not implemented yet")
    }
    else stop("Other projections than lonlat and polar not implemented yet")
    
    return(ret)
  }