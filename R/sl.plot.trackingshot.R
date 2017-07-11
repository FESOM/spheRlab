sl.trackingshot <- 
  function(way, projection = "lonlat"){
  
  }

sl.trackingshot.waypoint <-
  function(projection, time, lonlat.lonrange=NULL,lonlat.latrange=NULL,polar.lonlatrot=NULL,polar.latbound=NULL,regpoly.lonlatrot=NULL,regpoly.N=NULL,regpoly.lat0=NULL,regpoly.rotfrac=NULL) {
    
    if(!is.character(projection) || !is.numeric(time)) stop("Projection has to be a string and time a decimal number!")
    
    lonlat.full = !is.null(lonlat.lonrange) && !is.null(lonlat.latrange)
    polar.full = !is.null(polar.lonlatrot) && !is.null(polar.latbound)
    regpoly.full = !is.null(regpoly.lonlatrot) && !is.null(regpoly.N) && !is.null(regpoly.lat0) && !is.null(regpoly.rotfrac)
    
    ret = list("projection" = projection, time = time);
    class(ret) <- "sl.waypoint"
    
    if(projection == "lonlat" && lonlat.full){
      if(length(lonlat.lonrange) != 2 || length(lonlat.latrange) != 2){
        stop("Input sizes of lonlat args have to be 2 for each")
      }
      ret$lonlat.lonrange = lonlat.lonrange
      ret$lonlat.latrange = lonlat.latrange
      return(ret)
    }
    if(projection == "polar" && polar.full){
      stop("polar not implemented yet!")
    }
    else stop("Other projections than lonlat and polar not implemented yet")
  }

sl.trackingshot.waypoints <- 
  function(projection, ...) {
    arguments = list(...)
    for(a in arguments){
      if(class(a) != "sl.waypoint") stop("Argument must be of class sl.waypoint!")
      if(a$projection != projection) stop("Waypoints must have equal projection type!")
      a$projection = NULL
    }
    return(arguments)
  }

sl.trackingshot.way <-
  function(waypoints, times, smooth = F){
    if()
  }