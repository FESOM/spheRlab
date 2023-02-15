sl.grid2path <- 
  function (dat.time=NULL,
            dat=NULL,
            dat.files=NULL,
            dat.timeind=rep(1,length(dat.time)),
            dat.hastimedim=TRUE,
            dat.lon=NULL,
            dat.lat=NULL,
            traj.time=NULL,
            traj.lon=NULL,
            traj.lat=NULL,
            vars=NULL,
            max.dist=Inf,
            verbose=FALSE) {
    
    if (is.null(dat.time)) {stop("'dat.time' must be provided")}
    if (is.null(dat)) {
      from.files = TRUE
      if (is.null(dat.files)) {stop("one of 'dat' and 'dat.files' must be provided")}
      if (length(dat.timeind) != length(dat.time)) {stop("'dat.time' and 'dat.timeind' must have the same length")}
      require(ncdf4)
      dat.files.exist = file.exists(dat.files)
      if (!any(!is.na(dat.files.exist))) {stop("none of the files in 'dat.files' exists")}
    } else {
      from.files = FALSE
      if (!is.null(dat.files)) {warning("ignoring 'dat.files' (and 'dat.timeind') as 'dat' is provided")}
      if (!identical(dim(dat)[1:2], dim(dat.lon))) {stop("first two dimensions of 'dat' must be identical with the dimensions of 'dat.lon'")}
      if (length(dim(dat)) == 2) {
        if (dat.hastimedim) {stop("'dat' has only two dimensions, which is inconsistent with 'dat.hastimedim=TRUE'")}
      } else if (length(dim(dat)) == 3) {
        if (!dat.hastimedim) {stop("'dat' has three dimensions, which is inconsistent with 'dat.hastimedim=FALSE'")}
      } else {
        stop("'dat' must have two (without time) or three (with time) dimensions")
      }
      if (is.null(vars)) {
        vars = "var"
      } else {
        if (length(vars) > 1) {stop("when 'dat' is provided, only one variable is supported (and the single string in 'vars' only affects the output variable naming)")}
      }
    }
    if (length(dim(dat.lon)) != 2) {stop("'dat.lon' must be a matrix")}
    if (!identical(dim(dat.lon), dim(dat.lat))) {stop("'dat.lon' and 'dat.lat' must be equally-sized matrices")}
    
    traj.Nt = length(traj.time)
    dat.Nt = length(dat.time)
    
    # some argument checking
    if (!identical(traj.time, traj.time[order(traj.time)])) {
      stop("'traj.time' must be strictly monotonously increasing")
    }
    if (!identical(dat.time, dat.time[order(dat.time)])) {
      stop("'dat.time' must be strictly monotonously increasing")
    }
    
    vars.out = list()
    vars.N = length(vars)
    for (i in 1:vars.N) {vars.out[[i]] = rep(NA,traj.Nt)}
    names(vars.out) = vars
    nn.index = data.frame(row.index=rep(NA,traj.Nt),col.index=rep(NA,traj.Nt))
    nn.distance = rep(NA,traj.Nt)
    
    if (dat.Nt > 1) {
      traj.valid = which(traj.time >= dat.time[1] & traj.time <= tail(dat.time,1))
      if (length(traj.valid) == 0) {
        warning("no times in 'traj.time' are within the range of 'dat.time', returning only NAs")
        return(vars.out)
      }
      if (verbose) {"extracting data along a trajectory in a time-varying data field"}
    } else {
      traj.valid = 1:traj.Nt
      if (verbose) {"extracting data along a trajectory in a time-fixed data field"}
    }
    ###
    
    dat.xyz = sl.lonlat2xyz(lon = dat.lon, lat = dat.lat)
    dat.x = dat.xyz$x
    dat.y = dat.xyz$y
    dat.z = dat.xyz$z
    rm(dat.xyz)
    
    traj.xyz = sl.lonlat2xyz(lon = traj.lon, lat = traj.lat)
    traj.x = traj.xyz$x
    traj.y = traj.xyz$y
    traj.z = traj.xyz$z
    rm(traj.xyz)
    
    dat.Nrow = dim(dat.lon)[1]
    dat.Ncol = dim(dat.lon)[2]
    
    dat.ti = 2
    nn.inds = c(1,1)
    
    dat.files.prev = NULL
    dat.files.next = NULL
    vals.prev = rep(NA,vars.N)
    vals.next = rep(NA,vars.N)
    
    for (traj.ti in traj.valid) {
      
      ### skip time step if trajectory coordinate is NA
      if (is.na(traj.x[traj.ti])) {next}
      
      ### find nearest (next and previous) points in time
      traj.tm = traj.time[traj.ti]
      if (verbose) {print(paste0("Extracting data for trajectory time index ",traj.ti,", time ",traj.tm," ..."))}
      time.advanced = FALSE
      while (traj.tm > dat.time[dat.ti]) {
        if (verbose) {
          print(paste0("Time advanced beyond:"))
          print(dat.time[dat.ti])
        }
        dat.ti = dat.ti + 1
        time.advanced = TRUE
      }
      dat.wgh.prev = (dat.time[dat.ti] - traj.tm) / (dat.time[dat.ti] - dat.time[dat.ti-1])
      
      ### find nearest point in space
      prox.rows = (max(nn.inds[1]-1,1):min(nn.inds[1]+1,dat.Nrow))
      prox.cols = (max(nn.inds[2]-1,1):min(nn.inds[2]+1,dat.Ncol))
      dist.nn.prev = sl.gc.dist(x=data.frame(x1=traj.x[traj.ti],x2=dat.x[nn.inds[1],nn.inds[2]]),
                                y=data.frame(y1=traj.y[traj.ti],y2=dat.y[nn.inds[1],nn.inds[2]]),
                                z=data.frame(z1=traj.z[traj.ti],z2=dat.z[nn.inds[1],nn.inds[2]]),Rsphere = 6371)
      if (nn.inds[1] %in% c(1,dat.Nrow) || nn.inds[2] %in% c(1,dat.Ncol)) {
        # this means that the previous nearest neighbour is a marginal grid point
        # thus, to be safe, include the whole margin to check whether it's still the nearest neighbour
        x2 = c(as.vector(dat.x[prox.rows,prox.cols]),
               as.vector(dat.x[1,]),as.vector(dat.x[dat.Nrow,]),as.vector(dat.x[,1]),as.vector(dat.x[,dat.Ncol]))
        y2 = c(as.vector(dat.y[prox.rows,prox.cols]),
               as.vector(dat.y[1,]),as.vector(dat.y[dat.Nrow,]),as.vector(dat.y[,1]),as.vector(dat.y[,dat.Ncol]))
        z2 = c(as.vector(dat.z[prox.rows,prox.cols]),
               as.vector(dat.z[1,]),as.vector(dat.z[dat.Nrow,]),as.vector(dat.z[,1]),as.vector(dat.z[,dat.Ncol]))
      } else {
        x2 = as.vector(dat.x[prox.rows,prox.cols])
        y2 = as.vector(dat.y[prox.rows,prox.cols])
        z2 = as.vector(dat.z[prox.rows,prox.cols])
      }
      dat.dist.prox = matrix(sl.gc.dist(x=data.frame(x1=traj.x[traj.ti],x2=x2),
                                        y=data.frame(y1=traj.y[traj.ti],y2=y2),
                                        z=data.frame(z1=traj.z[traj.ti],z2=z2),Rsphere = 6371),
                             ncol=length(prox.cols))
      if (dist.nn.prev > min(dat.dist.prox)) {   # this means that the previous NN is not the NN anymore
        nn.res = sl.findnn.curvilin(x = traj.x[traj.ti], y = traj.y[traj.ti], z = traj.z[traj.ti],
                                    x.grid = dat.x, y.grid = dat.y, z.grid = dat.z, Rsphere = 6371)
        dist.nn = nn.res$nn.dist
        nn.inds = unlist(nn.res$nn.index)
        if (verbose) {
          print(paste0("Nearest neighbour changed to:"))
          print(nn.inds)
        }
      } else {
        dist.nn = dist.nn.prev
      }
      nn.index[traj.ti,] = nn.inds
      nn.distance[traj.ti] = dist.nn
      if (dist.nn > max.dist) {
        if (verbose) {print(paste0("NAs generated as path is too far from the nearest neighbour grid point (",dist.nn,"km > ",max.dist,"km)"))}
        next
      }
      
      ### read, compute and store value(s)
      if (time.advanced || is.null(dat.files.prev) || (dat.files[dat.ti-1] != dat.files.prev) || any(nn.inds != nn.index[traj.ti-1,])) {
        if (verbose) {print(paste0("Reading new data (gridded data time or location index advanced) ..."))}
        if (from.files) {
          dat.files.prev.prev = dat.files.prev
          dat.files.prev = dat.files[dat.ti-1]
          read.prev = FALSE
          val.prev = NA
          dat.files.next.prev = dat.files.next
          dat.files.next = dat.files[dat.ti]
          read.next = FALSE
          val.next = NA
          if (dat.files.exist[dat.ti-1]) {
            if (is.null(dat.files.prev.prev) || dat.files.prev != dat.files.prev.prev) {
              if (exists("fl.prev")) {nc_close(fl.prev)}
              if (!is.null(dat.files.next.prev) && dat.files.prev == dat.files.next.prev) {
                fl.prev = fl.next
              } else {
                if (verbose) {print(paste0("Opening NetCDF file ",dat.files.prev))}
                fl.prev = nc_open(dat.files.prev)
              }
            }
            read.prev = TRUE
          }
          if (dat.files.exist[dat.ti]) {
            if (is.null(dat.files.next.prev) || dat.files.next != dat.files.next.prev) {
              if (exists("fl.next") && dat.files.next.prev != dat.files.prev) {nc_close(fl.next)}
              if (verbose) {print(paste0("Opening NetCDF file ",dat.files.next))}
              fl.next = nc_open(dat.files.next)
            }
            read.next = TRUE
          }
          for (i.var in 1:vars.N) {
            if (!dat.hastimedim) {
              if (read.prev) {val.prev = ncvar_get(fl.prev, varid = vars[i.var], start = nn.inds, count = c(1,1))}
              if (read.next) {val.next = ncvar_get(fl.next, varid = vars[i.var], start = nn.inds, count = c(1,1))}
            } else {
              if (read.prev) {val.prev = ncvar_get(fl.prev, varid = vars[i.var], start = c(nn.inds,dat.timeind[dat.ti-1]), count = c(1,1,1))}
              if (read.next) {val.next = ncvar_get(fl.next, varid = vars[i.var], start = c(nn.inds,dat.timeind[dat.ti]), count = c(1,1,1))}
            }
            if (!read.prev) {
              if (dat.wgh.prev == 0) {val.prev = 0} else if (verbose && i.var == 1) {print(paste0("NAs generated as previous file ",dat.files.prev," does not exist"))}
            }
            if (!read.next) {
              if (dat.wgh.prev == 1) {val.next = 0} else if (verbose && i.var == 1) {print(paste0("NAs generated as next file ",dat.files.next," does not exist"))}
            }
          }
          vars.out[[i.var]][traj.ti] = val.prev * dat.wgh.prev + val.next * (1 - dat.wgh.prev)
          vals.prev[i.var] = val.prev
          vals.next[i.var] = val.next
        } else {
          if (!dat.hastimedim) {
            val.prev = dat[nn.inds[1],nn.inds[2]]
            val.next = val.prev
          } else {
            val.prev = dat[nn.inds[1],nn.inds[2],dat.ti-1]
            val.next = dat[nn.inds[1],nn.inds[2],dat.ti]
          }
          vars.out[[1]][traj.ti] = val.prev * dat.wgh.prev + val.next * (1 - dat.wgh.prev)
          vals.prev[1] = val.prev
          vals.next[1] = val.next
        }
      } else {
        for (i.var in 1:vars.N) {
          val.prev = vals.prev[i.var]
          val.next = vals.next[i.var]
          if (!read.prev) {
            if (dat.wgh.prev == 0) {val.prev = 0} else if (verbose && i.var == 1) {print(paste0("NAs generated as previous file ",dat.files.prev," does not exist"))}
          }
          if (!read.next) {
            if (dat.wgh.prev == 1) {val.next = 0} else if (verbose && i.var == 1) {print(paste0("NAs generated as next file ",dat.files.next," does not exist"))}
          }
          vars.out[[i.var]][traj.ti] = val.prev * dat.wgh.prev + val.next * (1 - dat.wgh.prev)
        }
      }
      
    }
    
    if (from.files) {
      if (exists("fl.prev")) {nc_close(fl.prev)}
      if (exists("fl.next")) {nc_close(fl.next)}
    }
    
    return(list(data=vars.out,nn.index=nn.index,nn.distance=nn.distance))
    
  }