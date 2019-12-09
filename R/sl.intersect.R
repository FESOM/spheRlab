sl.intersect <-
function(lon1,lat1,lon2=NULL,lat2=NULL,close1=FALSE,close2=FALSE,use.dist=TRUE,dist1=NULL,dist2=NULL,distmat=NULL,return.on.firsthit=FALSE) {
  
  N1 = length(lon1)
  if (is.null(lon2)) {singlepoly = TRUE} else {singlepoly = FALSE}
  
  anylines.intersect = FALSE
  lines.intersect = NULL
  lon = NULL
  lat = NULL
  
  if (singlepoly && N1 < 4) {
    return(list(anylines.intersect=anylines.intersect,lines.intersect=lines.intersect,lon=lon,lat=lat))
  }
  
  if (length(lat1) != N1) {stop("'lat1' must have the same length as 'lon1'")}
  if (close1) {
    lon1 = c(lon1, lon1[1])
    lat1 = c(lat1, lat1[1])
    N1d = N1
  } else {
    N1d = N1 - 1
  }
  if (use.dist) {
    if (is.null(dist1)) {
      dist1 = sl.gc.dist(lon1,lat1,sequential=TRUE)
    } else {
      if (length(dist1) != N1d) {stop("'dist1' must have have a length consistent with 'lon1' (depending on 'close1')")}
    }
  }
  
  if (singlepoly) {
    
    N2 = N1
    N2d = N1d
    lon2 = lon1
    lat2 = lat1
    dist2 = dist1
    
  } else {
    
    N2 = length(lon2)
    if (length(lat2) != N2) {stop("'lat2' must have the same length as 'lon2'")}
    if (close2) {
      lon2 = c(lon2, lon2[1])
      lat2 = c(lat2, lat2[1])
      N2d = N2
    } else {
      N2d = N2 - 1
    }
    if (use.dist) {
      if (is.null(dist2)) {
        dist2 = sl.gc.dist(lon2,lat2,sequential=TRUE)
      } else {
        if (length(dist2) != N2d) {stop("'dist2' must have have a length consistent with 'lon2' (depending on 'close2')")}
      }
    }
    
  }
  
  if (use.dist) {
    if (is.null(distmat)) {
      distmat = sl.finddist(lon1[1:N1],lat1[1:N1],lon2[1:N2],lat2[1:N2],return.vectors=FALSE,return.alldist=TRUE)$dist.all
    } else {
      if (dim(distmat) != c(N1,N2)) {stop("'distmat' must have have a length consistent with 'lon1' and 'lon2'")}
    }
  }
    
  for (i in 1:N1d) {
    
    if (singlepoly) {
      which.check.0 = rep(TRUE,N1d)
      which.check.0[1:min(i+1,N1d)] = FALSE
      if (i == 1 && close1) {which.check.0[N1d] = FALSE}
      which.check.0 = which(which.check.0)
    } else {
      which.check.0 = 1:N2d
    }
    
    if (use.dist) {
      which.check.1 = which.check.0 %% N2 + 1
      mindist = pmin(distmat[i,which.check.0],distmat[(i%%N1+1),which.check.0],distmat[i,which.check.1],distmat[(i%%N1+1),which.check.1])
      which.check = which.check.0[which(dist1[i] + dist2[which.check.0] > mindist * 2)]
    } else {
      which.check = which.check.0
    }
    
    for (j in which.check) {
      lli.res = sl.line.line.intersect(lon1[c(i,i+1)],lat1[c(i,i+1)],lon2[c(j,j+1)],lat2[c(j,j+1)])
      if (lli.res$lines.intersect) {
        anylines.intersect = TRUE
        lines.intersect = rbind(lines.intersect,c(i,j))
        lon = c(lon,lli.res$lon)
        lat = c(lat,lli.res$lat)
        if (return.on.firsthit) {
          return(list(anylines.intersect=anylines.intersect,lines.intersect=lines.intersect,lon=lon,lat=lat))
        }
      }
    }
    
  }
  
  return(list(anylines.intersect=anylines.intersect,lines.intersect=lines.intersect,lon=lon,lat=lat))
	
}
