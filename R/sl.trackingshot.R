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
    imgdir = "tmpSpherlabImg"
    if(!dir.exists(file.path(getwd(), imgdir))) dir.create(file.path(getwd(), imgdir))
    
    height = round(width*9/16)
    registerDoMC(threads)
    
    foreach(i = 1:imagecount) %dopar% {
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
      filename = file.path(getwd(), imgdir, sprintf(paste0("img_%0", floor(log10(imagecount))+1, "d.png"), i))
      
      pir = sl.plot.init(projection = projection, lonlat.lonrange = w$lonlat.lonrange, lonlat.latrange = w$lonlat.latrange, polar.lonlatrot = w$polar.lonlatrot, polar.latbound = w$polar.latbound, regpoly.lonlatrot = w$regpoly.lonlatrot, regpoly.N = w$regpoly.N, regpoly.lat0 = w$regpoly.lat0, regpoly.rotfrac = w$regpoly.rotfrac, col.background="white",main="",xshift=0,yshift=0,do.init=TRUE,file.name=sprintf(filename, i),width=width, png = T)
      sl.plot.field.elem(plot.init.res=pir, num=num, lon=lon, lat=lat, elem=elem, col.fill = col.fill, col.border = col.border, colbar = colbar, colbar.breaks = colbar.breaks, border.lwd = border.lwd, border.lty = border.lty)
      sl.plot.end(plot.init.res=pir)
      
      system(paste0(ffmpeg, " -y -i ", filename, " -vf \"scale=max(", width, "\\,a*", height, "):max(", height, "\\,", width, "/a),crop=", width, ":", height, "\" ", filename))
    }
    
    cmd = paste0(ffmpeg, " -y -framerate ", fps, " -s ", paste0(width, "x", round(width/(16/9))), " -i ", file.path(getwd(), imgdir, paste0("img_%0", floor(log10(imagecount))+1, "d.png")), " -pix_fmt yuv420p ", file.name)
    system(cmd)
    if(delete.images) unlink(x = file.path(getwd(), imgdir), recursive = T, force = T)
  }