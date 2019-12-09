sl.polygon.mergehole <-
function(poly,poly.hole,connect.maxstep=Inf,checkcross=TRUE,split.poly=FALSE,split.costexp=2) {
  
  if (any(!(c("lon","lat") %in% names(poly.hole)))) {
    stop("handling more than one hole at a time has not yet been implemented")
  }
  
  o.N = length(poly$lon)
  dupl = which(poly$lon == c(poly$lon[o.N],poly$lon[1:(o.N-1)])
               & poly$lat == c(poly$lat[o.N],poly$lat[1:(o.N-1)]))
  if (length(dupl) > 0) {
    warning(paste0("removing ",length(dupl)," duplicate points from 'poly'"))
    poly$lon = poly$lon[-dupl]
    poly$lat = poly$lat[-dupl]
    o.N = length(poly$lon)
  }
  
  if (checkcross) {
    intersect.res = sl.intersect(poly$lon,poly$lat,close1=TRUE,return.on.firsthit=TRUE)
    if (intersect.res$anylines.intersect) {stop("'poly' contains intersecting segments")}
  }
  
  o.lon.ext = c(poly$lon[o.N], poly$lon, poly$lon[1])
  o.lat.ext = c(poly$lat[o.N], poly$lat, poly$lat[1])
  
  o.angles = numeric(o.N)
  for (i in 1:o.N) {
    o.angles[i] = sl.angle(o.lon.ext[i:(i+2)],o.lat.ext[i:(i+2)],left=TRUE)
  }
  if (sum(o.angles) > o.N*pi) {
    poly$lon = poly$lon[o.N:1]
    poly$lat = poly$lat[o.N:1]
    o.angles = 2*pi - o.angles[o.N:1]
  }
  
  i.N = length(poly.hole$lon)
  dupl = which(poly.hole$lon == c(poly.hole$lon[i.N],poly.hole$lon[1:(i.N-1)])
               & poly.hole$lat == c(poly.hole$lat[i.N],poly.hole$lat[1:(i.N-1)]))
  if (length(dupl) > 0) {
    warning(paste0("removing ",length(dupl)," duplicate points from 'poly.hole'"))
    poly.hole$lon = poly.hole$lon[-dupl]
    poly.hole$lat = poly.hole$lat[-dupl]
    i.N = length(poly.hole$lon)
  }
  
  if (checkcross) {
    intersect.res = sl.intersect(poly.hole$lon,poly.hole$lat,close1=TRUE,return.on.firsthit=TRUE)
    if (intersect.res$anylines.intersect) {stop("'poly.hole' contains intersecting segments")}
  }
  
  i.lon.ext = c(poly.hole$lon[i.N], poly.hole$lon, poly.hole$lon[1])
  i.lat.ext = c(poly.hole$lat[i.N], poly.hole$lat, poly.hole$lat[1])
  
  i.angles = numeric(i.N)
  for (i in 1:i.N) {
    i.angles[i] = sl.angle(i.lon.ext[i:(i+2)],i.lat.ext[i:(i+2)],left=TRUE)
  }
  if (sum(i.angles) < i.N*pi) {
    poly.hole$lon = poly.hole$lon[i.N:1]
    poly.hole$lat = poly.hole$lat[i.N:1]
    i.angles = 2*pi - i.angles[i.N:1]
  }
  
  dist.res = sl.finddist(poly$lon,poly$lat,poly.hole$lon,poly.hole$lat,return.alldist=TRUE)
  o.ind = dist.res$ind[1]
  i.ind = dist.res$ind[2]
  connect.dist = dist.res$dist
  o.dist = NULL
  
  repeat {
    
    o.inds = NULL
    if (o.ind < o.N) {o.inds = (o.ind+1):o.N}
    if (o.ind > 1) {o.inds = c(o.inds,1:(o.ind-1))}
    
    i.inds = NULL
    if (i.ind < i.N) {i.inds = (i.ind+1):i.N}
    if (i.ind > 1) {i.inds = c(i.inds,1:(i.ind-1))}
    
    if (connect.dist == 0) {break}
    
    i.ang = sl.angle(c(i.lon.ext[c(i.ind,i.ind+1)],poly$lon[o.ind]),
                     c(i.lat.ext[c(i.ind,i.ind+1)],poly$lat[o.ind]),left=TRUE)
    o.ang = sl.angle(c(o.lon.ext[c(o.ind,o.ind+1)],poly.hole$lon[i.ind]),
                     c(o.lat.ext[c(o.ind,o.ind+1)],poly.hole$lat[i.ind]),left=TRUE)
    
    angles.OK = (i.ang >= 0 && i.ang <= i.angles[i.ind] && o.ang >= 0 && o.ang <= o.angles[o.ind])
    nocross = TRUE
    if (angles.OK && checkcross) {
      
      if (is.null(o.dist)) {
        o.dist = sl.gc.dist(o.lon.ext[2:(o.N+2)], o.lat.ext[2:(o.N+2)], sequential=TRUE)
        i.dist = sl.gc.dist(i.lon.ext[2:(i.N+2)], i.lat.ext[2:(i.N+2)], sequential=TRUE)
      }
      
      connect.lon = c(poly.hole$lon[i.ind],poly$lon[o.ind])
      connect.lat = c(poly.hole$lat[i.ind],poly$lat[o.ind])
      intersect.res = sl.intersect(connect.lon, connect.lat, poly.hole$lon[i.inds], poly.hole$lat[i.inds],
                                   dist1 = connect.dist, dist2 = i.dist[i.inds[-length(i.inds)]],
                                   return.on.firsthit = TRUE)
      if (!intersect.res$anylines.intersect) {
        intersect.res = sl.intersect(connect.lon, connect.lat, poly$lon[o.inds], poly$lat[o.inds],
                                     dist1 = connect.dist, dist2 = o.dist[o.inds[-length(o.inds)]],
                                     return.on.firsthit = TRUE)
      }
      nocross = !intersect.res$anylines.intersect
      
    }
    
    if (angles.OK && nocross) {break}
    
    dist.res$dist.all[o.ind,i.ind] = NA
    connect.dist = min(dist.res$dist.all,na.rm=TRUE)
    oi.inds = which(dist.res$dist.all == connect.dist, arr.ind = TRUE)[1,]
    o.ind = oi.inds[1]
    i.ind = oi.inds[2]
    
  }
  
  if (is.function(connect.maxstep)) {
    if (is.null(o.dist)) {
      o.dist = sl.gc.dist(o.lon.ext[2:(o.N+2)], o.lat.ext[2:(o.N+2)], sequential=TRUE)
      i.dist = sl.gc.dist(i.lon.ext[2:(i.N+2)], i.lat.ext[2:(i.N+2)], sequential=TRUE)
    }
    connect.maxstep = connect.maxstep(c(o.dist,i.dist))
  }
  
  if (connect.dist > connect.maxstep) {
    np = ceiling(connect.dist/connect.maxstep) - 1
    connect.insert = sl.fillequidist(c(poly$lon[o.ind],poly.hole$lon[i.ind]),
                                     c(poly$lat[o.ind],poly.hole$lat[i.ind]),np=np,method="gc")
    connect.insert.inv = list(lon=connect.insert$lon[np:1],lat=connect.insert$lat[np:1])
  } else {
    connect.insert = list(lon=NULL,lat=NULL)
    connect.insert.inv = list(lon=NULL,lat=NULL)
  }
  
  if (!split.poly) {
    lon = c(poly$lon[c(o.ind,o.inds,o.ind)], connect.insert$lon,
            poly.hole$lon[c(i.ind,i.inds,i.ind)], connect.insert.inv$lon)
    lat = c(poly$lat[c(o.ind,o.inds,o.ind)], connect.insert$lat,
            poly.hole$lat[c(i.ind,i.inds,i.ind)], connect.insert.inv$lat)
    return(list(lon=lon, lat=lat))
  }
  
  o.ind.prev = o.ind
  i.ind.prev = i.ind
  connect.dist.prev = connect.dist
  if (checkcross) {
    connect.lon.prev = connect.lon
    connect.lat.prev = connect.lat
  }
  connect.insert.prev = connect.insert
  connect.insert.inv.prev = connect.insert.inv
  
  o.dist.cumsum = cumsum(o.dist)
  o.dist.total = tail(o.dist.cumsum,1)
  o.dist.cumsum = (o.dist.cumsum - o.dist.cumsum[o.ind.prev])[-o.ind.prev]
  o.dist.min = pmin(o.dist.total-o.dist.cumsum, abs(o.dist.cumsum), o.dist.total+o.dist.cumsum)

  i.dist.cumsum = cumsum(i.dist)
  i.dist.total = tail(i.dist.cumsum,1)
  i.dist.cumsum = (i.dist.cumsum - i.dist.cumsum[i.ind.prev])[-i.ind.prev]
  i.dist.min = pmin(i.dist.total-i.dist.cumsum, abs(i.dist.cumsum), i.dist.total+i.dist.cumsum)
  
  split.costfun = dist.res$dist.all[-o.ind.prev,-i.ind.prev] / (pmin(matrix(o.dist.min,nrow=o.N-1,ncol=i.N-1),
                                                           matrix(i.dist.min,nrow=o.N-1,ncol=i.N-1,byrow=TRUE)))^split.costexp
  
  oi.inds = which(split.costfun == min(split.costfun,na.rm=TRUE), arr.ind = TRUE)[1,]
  o.ind = oi.inds[1]
  if (o.ind >= o.ind.prev) {o.ind = o.ind + 1}
  i.ind = oi.inds[2]
  if (i.ind >= i.ind.prev) {i.ind = i.ind + 1}
  connect.dist = dist.res$dist.all[o.ind,i.ind]
  
  repeat {
    
    o.inds = NULL
    if (o.ind < o.N) {o.inds = (o.ind+1):o.N}
    if (o.ind > 1) {o.inds = c(o.inds,1:(o.ind-1))}
    
    i.inds = NULL
    if (i.ind < i.N) {i.inds = (i.ind+1):i.N}
    if (i.ind > 1) {i.inds = c(i.inds,1:(i.ind-1))}
    
    if (connect.dist == 0) {break}
    
    i.ang = sl.angle(c(i.lon.ext[c(i.ind,i.ind+1)],poly$lon[o.ind]),
                     c(i.lat.ext[c(i.ind,i.ind+1)],poly$lat[o.ind]),left=TRUE)
    o.ang = sl.angle(c(o.lon.ext[c(o.ind,o.ind+1)],poly.hole$lon[i.ind]),
                     c(o.lat.ext[c(o.ind,o.ind+1)],poly.hole$lat[i.ind]),left=TRUE)
    
    angles.OK = (i.ang >= 0 && i.ang <= i.angles[i.ind] && o.ang >= 0 && o.ang <= o.angles[o.ind])
    nocross = TRUE
    if (angles.OK && checkcross) {
      
      connect.lon = c(poly.hole$lon[i.ind],poly$lon[o.ind])
      connect.lat = c(poly.hole$lat[i.ind],poly$lat[o.ind])
      intersect.res = sl.intersect(connect.lon, connect.lat, poly.hole$lon[i.inds], poly.hole$lat[i.inds],
                                   dist1 = connect.dist, dist2 = i.dist[i.inds[-length(i.inds)]],
                                   return.on.firsthit = TRUE)
      if (!intersect.res$anylines.intersect) {
        intersect.res = sl.intersect(connect.lon, connect.lat, poly$lon[o.inds], poly$lat[o.inds],
                                     dist1 = connect.dist, dist2 = o.dist[o.inds[-length(o.inds)]],
                                     return.on.firsthit = TRUE)
      }
      if (!intersect.res$anylines.intersect) {
        intersect.res = sl.intersect(connect.lon, connect.lat, connect.lon.prev, connect.lat.prev,
                                     dist1 = connect.dist, dist2 = connect.dist.prev,
                                     return.on.firsthit = TRUE)
      }
      nocross = !intersect.res$anylines.intersect
      
    }
    
    if (angles.OK && nocross) {break}
    
    split.costfun[oi.inds[1],oi.inds[2]] = NA
    oi.inds = which(split.costfun == min(split.costfun,na.rm=TRUE), arr.ind = TRUE)[1,]
    o.ind = oi.inds[1]
    if (o.ind >= o.ind.prev) {o.ind = o.ind + 1}
    i.ind = oi.inds[2]
    if (i.ind >= i.ind.prev) {i.ind = i.ind + 1}
    connect.dist = dist.res$dist.all[o.ind,i.ind]
    
  }
  
  if (connect.dist > connect.maxstep) {
    np = ceiling(connect.dist/connect.maxstep) - 1
    connect.insert = sl.fillequidist(c(poly$lon[o.ind],poly.hole$lon[i.ind]),
                                     c(poly$lat[o.ind],poly.hole$lat[i.ind]),np=np,method="gc")
    connect.insert.inv = list(lon=connect.insert$lon[np:1],lat=connect.insert$lat[np:1])
  } else {
    connect.insert = list(lon=NULL,lat=NULL)
    connect.insert.inv = list(lon=NULL,lat=NULL)
  }
  
  o.inds.cut = which(o.inds == o.ind.prev)
  o.inds.1 = NULL
  if (o.inds.cut < (o.N-1)) {o.inds.1 = o.inds[(o.inds.cut+1):(o.N-1)]}
  o.inds.2 = NULL
  if (o.inds.cut > 1) {o.inds.2 = o.inds[1:(o.inds.cut-1)]}
  
  i.inds.cut = which(i.inds == i.ind.prev)
  i.inds.1 = NULL
  if (i.inds.cut > 1) {i.inds.1 = i.inds[1:(i.inds.cut-1)]}
  i.inds.2 = NULL
  if (i.inds.cut < (i.N-1)) {i.inds.2 = i.inds[(i.inds.cut+1):(i.N-1)]}
  
  lon1 = c(poly$lon[c(o.ind.prev,o.inds.1,o.ind)], connect.insert$lon,
          poly.hole$lon[c(i.ind,i.inds.1,i.ind.prev)], connect.insert.inv.prev$lon)
  lat1 = c(poly$lat[c(o.ind.prev,o.inds.1,o.ind)], connect.insert$lat,
           poly.hole$lat[c(i.ind,i.inds.1,i.ind.prev)], connect.insert.inv.prev$lat)
  lon2 = c(poly$lon[c(o.ind,o.inds.2,o.ind.prev)], connect.insert.prev$lon,
           poly.hole$lon[c(i.ind.prev,i.inds.2,i.ind)], connect.insert.inv$lon)
  lat2 = c(poly$lat[c(o.ind,o.inds.2,o.ind.prev)], connect.insert.prev$lat,
           poly.hole$lat[c(i.ind.prev,i.inds.2,i.ind)], connect.insert.inv$lat)
  return(list(polygon1=list(lon=lon1, lat=lat1), polygon2=list(lon=lon2, lat=lat2)))
	
}
