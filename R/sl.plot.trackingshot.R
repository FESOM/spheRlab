sl.trackingshot <- 
  function(way, num, lon, lat, elem, fps = 25L, time = NA, ffmpeg = Sys.which("ffmpeg"), col.background="white", col.fill="colbar", col.border="colbar", colbar=sl.colbar.redgreyblue_256, colbar.breaks=NA, border.lwd=0.01, border.lty=1, file.name = "video.mp4", width=1080, threads = 1, delete.images = T){
    if(!is.list(way) || class(way) != "sl.way") stop("way has to be a list of class 'sl.way'")
    if(!is.integer(fps)) stop("fps has to be integer")
    if(length(fps) > 1) fps = fps[1]
    if(fps < 1L) stop("fps has to be greater than 0")
    if(!file.exists(ffmpeg)) stop("ffmpeg executable does not exist")
    if(!is.numeric(time) && !is.na(time)) stop("time has to be numeric value or NA")
    if(length(time) > 1) time = time[1]
    if(!is.na(time) && time < 0) stop("time must be greater than 0")
    if(!is.list(way$waypoints) || class(way$waypoints) != "sl.waypoints") stop("way$waypoints has to be a list of class 'sl.waypoints'")
    if(!is.numeric(way$time)) stop("time has to be a numeric vector")
    if(any(way$time < 0)) stop("way$times have to be at least 0")
    if(length(way$waypoints) > length(way$time) + 1) stop("way$times should contain at least as much times as are needed to fill the gaps between way$waypoints")
    if(!is.character(way$projection)) stop("way$projection must be a string")
    for(w in way$waypoints){
      if(!is.list(w) || class(w) != "sl.waypoint") stop("Waypoints entry must be list and of class 'sl.waypoint'")
      if(!is.numeric(w$time)) stop("time of waypoint must be numeric")
      if(length(w$time) > 1) w$time = w$time[1]
      if(w$time < 0) stop("time must be a positive number")
    }
    
    projection = way$projection
    
    time.factor = 1
    waytime = 0
    for(w in way$waypoints){
      waytime = waytime + w$time
    }
    waytime = waytime + sum(way$times)
    waytime = ceiling(waytime * fps)/fps
    time = ceiling(time * fps)/fps
    if(!is.na(time)) time.factor = time / waytime
    if(time.factor != 1){
      for(w in way$waypoints){
        w$time = w$time * time.factor
      }
      way$times = way$times * time.factor
      waytime = waytime * time.factor
    }
    
    imagecount = round(waytime * fps)
    print(imagecount)
    imgdir = "tmpSpherlabImg"
    if(!dir.exists(file.path(getwd(), imgdir))) dir.create(file.path(getwd(), imgdir))
    
    registerDoMC(threads)
    
    foreach(i = 1:imagecount) %dopar% {
      print("run")
      print(i)
      imgtime = i / fps
      counter = -1
      temp.time = 0
      temp.time.old = 0
      while(temp.time < imgtime){
        counter = counter + 1
        temp.time.old = temp.time
        if(counter %% 2 == 0){
          temp.time = temp.time + way$waypoints[[floor(counter/2)+1]]$time
        }
        else {
          temp.time = temp.time + way$times[floor(counter/2)+1]
        }
      }
      w = list()
      if(counter %% 2 == 0) w = way$waypoints[[floor(counter/2)+1]]
      else {
        w1 = way$waypoints[[floor(counter/2)+1]]
        w2 = way$waypoints[[floor(counter/2)+2]]
        w = w1
        ft = (imgtime-temp.time.old)/(temp.time-temp.time.old)
        if(projection == "lonlat"){
          w$lonlat.lonrange = (w2$lonlat.lonrange - w1$lonlat.lonrange)*ft+w1$lonlat.lonrange
          w$lonlat.latrange = (w2$lonlat.latrange - w1$lonlat.latrange)*ft+w1$lonlat.latrange
        }
      }
      
      pir = sl.plot.init(projection = projection, lonlat.lonrange = w$lonlat.lonrange, lonlat.latrange = w$lonlat.latrange, polar.lonlatrot = w$polar.lonlatrot, polar.latbound = w$polar.latbound, regpoly.lonlatrot = w$regpoly.lonlatrot, regpoly.N = w$regpoly.N, regpoly.lat0 = w$regpoly.lat0, regpoly.rotfrac = w$regpoly.rotfrac, col.background="white",main="",xshift=0,yshift=0,do.init=TRUE,file.name=file.path(getwd(), imgdir, sprintf(paste0("img_%0", floor(log10(imagecount))+1, "d.png"), i)),width=width, png = T)
      sl.plot.field.elem(plot.init.res=pir, num=num, lon=lon, lat=lat, elem=elem, col.fill = col.fill, col.border = col.border, colbar = colbar, colbar.breaks = colbar.breaks, border.lwd = border.lwd, border.lty = border.lty)
      sl.plot.end(plot.init.res=pir)
    }
    
    cmd = paste0(ffmpeg, " -framerate ", fps, " -s ", paste0(width, "x", round(width/(16/9))), " -i ", file.path(getwd(), imgdir, paste0("img_%0", floor(log10(imagecount))+1, "d.png")), " -pix_fmt yuv420p ", file.name)
    system(cmd)
    if(delete.images) unlink(x = file.path(getwd(), imgdir), recursive = T, force = T)
  }

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

sl.trackingshot.way <-
  function(waypoints, times, smooth.line = 0, smooth.time = 0, ignore.checks = F){
    if(!ignore.checks){
      if(!is.list(waypoints) || class(waypoints) != "sl.waypoints") stop("waypoints list has to be of class sl.waypoints!")
      if(!is.numeric(times)) stop("times has to be a numeric vector!")
      if(length(waypoints)-1 - length(times) > 1) stop("times should contain at least one element less than waypoints")
      if(!is.numeric(smooth.line)) {
        cat("smooth.line set to non-numeric value. resetting to default...\n")
        smooth.line = 0
      }
      if(length(smooth.line) > 1) smooth.line = smooth.line[1]
      if(!is.numeric(smooth.time)) {
        cat("smooth.time set to non-numeric value. resetting to default...\n")
        smooth.time = 0
      }
      if(length(smooth.time) > 1) smooth.time = smooth.time[1]
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