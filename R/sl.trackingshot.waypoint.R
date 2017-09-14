sl.trackingshot.waypoint <-
  function(projection, time = 0, lonlat.lonrange=c(-180,180),lonlat.latrange=c(-85,85),polar.lonlatrot=c(0,90,0),polar.latbound=0,regpoly.lonlatrot=c(0,90,0),regpoly.N=3,regpoly.lat0=60,regpoly.rotfrac=0) {
    
    if(!is.character(projection)) stop("projection has to be a string")
    if(!is.numeric(time)) stop("time has to be numeric")
    if(length(time) > 1) stop("time has to be a numeric vector of length 1")
    if(time < 0) stop("time has to be a positiv number")
    
    lonlat.full = !is.null(lonlat.lonrange) && !is.null(lonlat.latrange)
    polar.full = !is.null(polar.lonlatrot) && !is.null(polar.latbound)
    regpoly.full = !is.null(regpoly.lonlatrot) && !is.null(regpoly.N) && !is.null(regpoly.lat0) && !is.null(regpoly.rotfrac)
    
    ret = list("projection" = projection, time = time);
    class(ret) <- "sl.waypoint"
    
    if(projection == "lonlat"){
      if(!lonlat.full) stop("lonlat.* arguments must not be NULL")
      if(!is.numeric(lonlat.lonrange)) stop("lonlat.lonrange has to be a numeric vector")
      if(!is.numeric(lonlat.latrange)) stop("lonlat.latrange has to be a numeric vector")
      if(length(lonlat.lonrange) != 2) stop("length of lonlat.lonrange has to be 2")
      if(length(lonlat.latrange) != 2) stop("length of lonlat.latrange has to be 2")
      
      ret$lonlat.lonrange = lonlat.lonrange
      ret$lonlat.latrange = lonlat.latrange
    }
    else if(projection == "polar"){
      if(!polar.full) stop("polar.* arguments must not be NULL")
      if(!is.numeric(polar.lonlatrot)) stop("polar.lonlatrot has to be a numeric vector")
      if(length(polar.lonlatrot) < 3) stop("polar.lonlatrot needs length of 3")
      if(length(polar.lonlatrot) > 3) polar.lonlatrot = polar.lonlatrot[1:3]
      if(!is.numeric(polar.latbound)) stop("polar.latbound has to be a numeric vector")
      if(length(polar.latbound) > 1) polar.latbound = polar.latbound[1]
      
      ret$polar.lonlatrot = polar.lonlatrot
      ret$polar.latbound = polar.latbound
    }
    else if(projection == "regpoly"){
      if(!regpoly.full) stop("regpoly.* arguments must not be NULL")
      if(!is.numeric(regpoly.lonlatrot)) stop("regpoly.lonlatrot has to be a numeric vector")
      if(length(regpoly.lonlatrot) < 3) stop("regpoly.lonlatrot needs length of 3")
      if(length(regpoly.lonlatrot) > 3) regpoly.lonlatrot = regpoly.lonlatrot[1:3]
      if(!is.numeric(regpoly.N)) stop("regpoly.N has to be an integer")
      if(length(regpoly.N) > 1) regpoly.N = regpoly.N[1]
      if(!is.numeric(regpoly.lat0)) stop("regpoly.lat0 has to be a numeric vector")
      if(length(regpoly.lat0) > 1) regpoly.lat0 = regpoly.lat0[1]
      if(!is.numeric(regpoly.rotfrac)) stop("regpoly.rotfrac has to be a numeric vector")
      if(length(regpoly.rotfrac) > 1) regpoly.rotfrac = regpoly.rotfrac[1]
      
      ret$regpoly.lonlatrot = regpoly.lonlatrot
      ret$regpoly.N = regpoly.N
      ret$regpoly.lat0 = regpoly.lat0
      ret$regpoly.rotfrac = regpoly.rotfrac
    }
    else stop("Other projections than lonlat, polar and regpoly not implemented yet")
    
    return(ret)
  }