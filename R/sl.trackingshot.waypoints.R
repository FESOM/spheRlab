sl.trackingshot.waypoints <- 
  function(projection, ...) {
    waypoints = list(...)
    for(a in waypoints){
      if(!is.list(a) || class(a) != "sl.waypoint") stop("Arguments must be list and of class 'sl.waypoint'")
      if(a$projection != projection) stop("Waypoints must have equal projection type")
      if(!is.numeric(a$time)) stop("time of waypoint has to be numeric")
      if(length(a$time) > 1) a$time = a$time[1]
      if(a$time < 0) stop("time has to be a positive number")
      a$projection = NULL
    }
    class(waypoints) <- "sl.waypoints"
    waypoints$projection = projection
    return(waypoints)
  }