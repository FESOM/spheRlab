sl.trajectory.remaptime <- function (oldtime,oldlat,oldlon,newtime,method="linear",extrapolate=FALSE,extrapolate.maxspeed=Inf,return.remapinfo=FALSE,verbose=TRUE) {
  
  if (method != "nearestneighbour" && method != "linear") {
    stop("'method' must be one of 'nearestneighbour' and 'linear'.")
  }
  
  old.N = length(oldtime)
  new.N = length(newtime)
  
  if (new.N > 1) {
    if (any(newtime[2:new.N] <= newtime[1:(new.N-1)])) {stop("'newtime' must increase strict monotonously")}
  }
  if (old.N <= 1) {
    if (old.N < 1) {stop("'oldtime' must have at least one element")}
    warning("'oldtime' has only one single element; remapping results will be trivial")
    if (is.null(dim(oldlat))) {
      newlat = rep(NA,new.N)
      newlon = rep(NA,new.N)
      newlat[newtime==oldtime] = oldlat
      newlon[newtime==oldtime] = oldlon
      if (extrapolate) {
        newlat[newtime!=oldtime] = oldlat
        newlon[newtime!=oldtime] = oldlon
      }
    } else {
      nTraj = ncol(oldlat)
      newlat = matrix(nrow=new.N,ncol=nTraj)
      newlon = matrix(nrow=new.N,ncol=nTraj)
      newlat[newtime==oldtime,] = oldlat
      newlon[newtime==oldtime,] = oldlon
      if (extrapolate) {
        for (i.newtime in which(newtime!=oldtime)) {
          newlat[i.newtime,] = oldlat
          newlon[i.newtime,] = oldlon
        }
      }
    }
    if (return.remapinfo) {
      weights.left.ind = rep(1,new.N)
      weights.left = rep(1,new.N)
      if (!extrapolate) {
        weights.left.ind[newtime!=oldtime] = NA
        weights.left[newtime!=oldtime] = NA
      }
      return(list(Lat=newlat,Lon=newlon,remapinfo=list(weights.left.ind=weights.left.ind,weights.left=weights.left)))
    } else {return(list(Lat=newlat,Lon=newlon))}
  }
  if (any(oldtime[2:old.N] <= oldtime[1:(old.N-1)])) {stop("'oldtime' must increase strict monotonously")}
  
  if (newtime[new.N] > oldtime[old.N]) {
    if (newtime[1] > oldtime[old.N]) {
      if (verbose) {warning("New time axis completely out of original time range.")}
      if (extrapolate) {
        if (verbose) {warning("All values will be extrapolated.")}
      } else {
        if (verbose) {warning("All values will be 'NA'.")}
      }
    } else {
      if (verbose) {warning("New time axis ends later than original time range.")}
      if (extrapolate) {
        if (verbose) {warning("Values after original time range will be extrapolated.")}
      } else {
        if (verbose) {warning("Values after original time range will be 'NA'.")}
      }
    }
  }
  if (newtime[1] < oldtime[1]) {
    if (newtime[new.N] < oldtime[1]) {
      if (verbose) {warning("New time axis completely out of original time range.")}
      if (extrapolate) {
        if (verbose) {warning("All values will be extrapolated.")}
      } else {
        if (verbose) {warning("All values will be 'NA'.")}
      }
    } else {
      if (verbose) {warning("New time axis begins earlier than original time range.")}
      if (extrapolate) {
        if (verbose) {warning("Values before original time range will be extrapolated.")}
      } else {
        if (verbose) {warning("Values before original time range will be 'NA'.")}
      }
    }
  }
  
  weights.left = rep(NA,new.N)
  weights.left.ind = rep(NA,new.N)
  i.old = 1
  for (i.new in 1:new.N) {
    while (i.old < (old.N-1) && newtime[i.new] > oldtime[i.old+1]) {
      i.old = i.old + 1
    }
    weights.left.ind[i.new] = i.old
    weights.left[i.new] = (oldtime[i.old+1] - newtime[i.new]) / (oldtime[i.old+1] - oldtime[i.old])
  }
  if (!extrapolate) {
    weights.left[weights.left < 0 | weights.left > 1] = NA
  }
  if (method == "nearestneighbour") {
    weights.left[weights.left > .5] = 1
    weights.left[weights.left <= .5] = 0
  }
  
  if (is.null(dim(oldlat))) {
    newlat = rep(NA,new.N)
    newlon = rep(NA,new.N)
    if (extrapolate && extrapolate.maxspeed < Inf) {
      if (any(weights.left > 1)) {
        speed = sl.gc.dist(lon = oldlon[1:2], lat = oldlat[1:2]) / (oldtime[2] - oldtime[1])
        if (speed > extrapolate.maxspeed) {
          weights.left[weights.left > 1] = 1 + (weights.left[weights.left > 1] - 1) * extrapolate.maxspeed / speed
        }
      }
      if (any(weights.left < 0)) {
        speed = sl.gc.dist(lon = oldlon[(old.N-1):old.N], lat = oldlat[(old.N-1):old.N]) / (oldtime[old.N] - oldtime[old.N-1])
        if (speed > extrapolate.maxspeed) {
          weights.left[weights.left < 0] = weights.left[weights.left < 0] * extrapolate.maxspeed / speed
        }
      }
    }
    for (i in 1:new.N) {
      newlonlat = sl.p2p(lon1=oldlon[weights.left.ind[i]],lat1=oldlat[weights.left.ind[i]],
                         lon2=oldlon[weights.left.ind[i]+1],lat2=oldlat[weights.left.ind[i]+1],
                         frac=1-weights.left[i])
      newlat[i] = newlonlat$lat
      newlon[i] = newlonlat$lon
    }
  } else {
    nTraj = ncol(oldlat)
    newlat = matrix(nrow=new.N,ncol=nTraj)
    newlon = matrix(nrow=new.N,ncol=nTraj)
    if (extrapolate && extrapolate.maxspeed < Inf) {weights.left.nomaxspeed = weights.left}
    for (j in 1:nTraj) {
      if (extrapolate && extrapolate.maxspeed < Inf) {
        weights.left = weights.left.nomaxspeed
        if (any(weights.left > 1)) {
          speed = sl.gc.dist(lon = oldlon[1:2,j], lat = oldlat[1:2,j]) / (oldtime[2] - oldtime[1])
          if (speed > extrapolate.maxspeed) {
            weights.left[weights.left > 1] = 1 + (weights.left[weights.left > 1] - 1) * extrapolate.maxspeed / speed
          }
        }
        if (any(weights.left < 0)) {
          speed = sl.gc.dist(lon = oldlon[(old.N-1):old.N,j], lat = oldlat[(old.N-1):old.N,j]) / (oldtime[old.N] - oldtime[old.N-1])
          if (speed > extrapolate.maxspeed) {
            weights.left[weights.left < 0] = weights.left[weights.left < 0] * extrapolate.maxspeed / speed
          }
        }
      }
      for (i in 1:new.N) {
        newlonlat = sl.p2p(lon1=oldlon[weights.left.ind[i],j],lat1=oldlat[weights.left.ind[i],j],
                           lon2=oldlon[weights.left.ind[i]+1,j],lat2=oldlat[weights.left.ind[i]+1,j],
                           frac=1-weights.left[i])
        newlat[i,j] = newlonlat$lat
        newlon[i,j] = newlonlat$lon
      }
    }
  }
  
  if (return.remapinfo) {
    return(list(Lat=newlat,Lon=newlon,remapinfo=list(weights.left.ind=weights.left.ind,weights.left=weights.left)))
  } else {return(list(Lat=newlat,Lon=newlon))}
  
  
}