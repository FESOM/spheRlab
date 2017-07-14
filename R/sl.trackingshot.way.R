sl.trackingshot.way <-
  function(waypoints, times, smooth.line = 0, ignore.checks = F){
    if(!ignore.checks){
      if(!is.list(waypoints) || class(waypoints) != "sl.waypoints") stop("waypoints list has to be of class sl.waypoints!")
      if(!is.numeric(times)) stop("times has to be a numeric vector!")
      if(length(waypoints)-1 - length(times) > 1) stop("times should contain at least one element less than waypoints")
      if(!is.numeric(smooth.line)) {
        cat("smooth.line set to non-numeric value. resetting to default...\n")
        smooth.line = 0
      }
      if(length(smooth.line) > 1) smooth.line = smooth.line[1]
      if(!is.character(waypoints$projection)) stop("waypoints$projection must be a string")
      for(w in waypoints){
        if(!is.character(w)){
          if(!is.list(w) || class(w) != "sl.waypoint") stop("Waypoints entry must be list and of class 'sl.waypoint'")
          if(!is.numeric(w$time)) stop("time of waypoint must be numeric")
          if(length(w$time) > 1) w$time = w$time[1]
          if(w$time < 0) stop("time must be a positive number")
        }
      }
    }
    proj = waypoints$projection
    waypoints$projection = NULL
    way = list(projection = proj, waypoints = waypoints, times = times[1:(length(waypoints)-1)])
    class(way) = "sl.way"
    return(way)
  }