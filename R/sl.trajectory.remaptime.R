sl.trajectory.remaptime <- function (oldtime,oldlat,oldlon,newtime,method="linear",extrapolate=FALSE,return.remapinfo=FALSE) {
  
  if (method != "nearestneighbour" && method != "linear") {
    stop("'method' must be one of 'nearestneighbour' and 'linear'.")
  }
  
  old.N = length(oldtime)
  new.N = length(newtime)
  
  if (any(oldtime[2:old.N] <= oldtime[1:(old.N-1)])) {stop("'oldtime' must increase strict monotonously")}
  if (any(newtime[2:new.N] <= newtime[1:(new.N-1)])) {stop("'newtime' must increase strict monotonously")}
  
  if (newtime[new.N] > oldtime[old.N]) {
    if (newtime[1] > oldtime[old.N]) {
      warning("New time axis completely out of original time range.")
      if (extrapolate) {
        warning("All values will be extrapolated.")
      } else {
        warning("All values will be 'NA'.")
      }
    } else {
      warning("New time axis ends later than original time range.")
      if (extrapolate) {
        warning("Values after original time range will be extrapolated.")
      } else {
        warning("Values after original time range will be 'NA'.")
      }
    }
  }
  if (newtime[1] < oldtime[1]) {
    if (newtime[new.N] < oldtime[1]) {
      warning("New time axis completely out of original time range.")
      if (extrapolate) {
        warning("All values will be extrapolated.")
      } else {
        warning("All values will be 'NA'.")
      }
    } else {
      warning("New time axis begins earlier than original time range.")
      if (extrapolate) {
        warning("Values before original time range will be extrapolated.")
      } else {
        warning("Values before original time range will be 'NA'.")
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
  
  newlat = rep(NA,new.N)
  newlon = rep(NA,new.N)
  for (i in 1:new.N) {
    newlonlat = sl.p2p(lon1=oldlon[weights.left.ind[i]],lat1=oldlat[weights.left.ind[i]],
                       lon2=oldlon[weights.left.ind[i]+1],lat2=oldlat[weights.left.ind[i]+1],frac=1-weights.left[i])
    newlat[i] = newlonlat$lat
    newlon[i] = newlonlat$lon
  }
  
  if (return.remapinfo) {
    return(list(Lat=newlat,Lon=newlon,remapinfo=list(weights.left.ind=weights.left.ind,weights.left=weights.left)))
  } else {return(list(Lat=newlat,Lon=newlon))}
  
  
}