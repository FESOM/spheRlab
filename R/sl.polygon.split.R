sl.polygon.split <-
function(lon,lat,angles=NULL,dist.mat=NULL,mask.mat=NULL,split.costexp=2,split.costborderbygcdist=FALSE,split.maxstep=Inf,split.maxstep.passfun=TRUE,split.cleanup="none",checkcross.input=TRUE,checkcross.output=TRUE,checkfail.maskfrac=0.01,stop.maxiter=1,stop.maxpoints=3,stop.maxangle=pi,xyz=NULL) {
  
  N = length(lon)
  dupl = which(lon == c(lon[N],lon[1:(N-1)])
               & lat == c(lat[N],lat[1:(N-1)]))
  if (length(dupl) > 0) {
    #warning(paste0("removing ",length(dupl)," duplicate points from input polygon"))
    lon = lon[-dupl]
    lat = lat[-dupl]
    N = length(lon)
    if (!is.null(xyz)) {xyz = xyz[-dupl,]}
    if (!is.null(angles)) {
      warning("recomputing angles after removal of duplicate points")
      angles = NULL
    }
    if (!is.null(dist.mat)) {
      dist.mat = dist.mat[-dupl,-dupl]
    }
    if (!is.null(mask.mat)) {
      mask.mat = mask.mat[-dupl,-dupl]
    }
  }
  
  if (N < 3) {stop("Input polygon must contain at least three unique points")}
  
  if (checkcross.input) {
    intersect.res = sl.intersect(lon,lat,close1=TRUE,return.on.firsthit=TRUE)
    if (intersect.res$anylines.intersect) {stop("input polygon contains intersecting segments")}
  }
  
  lon.ext = c(lon[N], lon, lon[1])
  lat.ext = c(lat[N], lat, lat[1])
  
  if (is.null(angles)) {
    angles = numeric(N)
    for (i in 1:N) {
      angles[i] = sl.angle(lon.ext[i:(i+2)],lat.ext[i:(i+2)],left=TRUE)
    }
    if (sum(angles) > N*pi) {
      lon = lon[N:1]
      lat = lat[N:1]
      lon.ext = c(lon[N], lon, lon[1])
      lat.ext = c(lat[N], lat, lat[1])
      if (!is.null(dist.mat)) {dist.mat = dist.mat[N:1,N:1]}
      if (!is.null(mask.mat)) {mask.mat = mask.mat[N:1,N:1]}
      angles = 2*pi - angles[N:1]
      if (!is.null(xyz)) {xyz = xyz[N:1,]}
    }
  }
  
  if (N == 3) {return(list(list(lon = lon, lat = lat)))}
  
  if (is.null(dist.mat)) {
    if (is.null(xyz)) {
      xyz = sl.lonlat2xyz(lon = lon, lat = lat)
      xyz = cbind(xyz$x,xyz$y,xyz$z)
    }
    dist.mat = as.matrix(2*asin(dist(xyz)/2))
  }
  offdiag = matrix(c(rep(c(rep(FALSE,N),TRUE),N-1),FALSE),nrow=N,ncol=N)
  if (is.null(mask.mat)) {
    mask.mat = offdiag
    mask.mat[1,N] = TRUE
    mask.mat[t(mask.mat)] = TRUE
    diag(mask.mat) = TRUE
  } else {
    mask.mat[1,N] = TRUE
    mask.mat[N,1] = TRUE
  }
  dist.border.pairwise = c(dist.mat[offdiag],dist.mat[N,1])
  if (split.costborderbygcdist) {
    dist.border = cumsum(dist.border.pairwise)
    dist.border.mat = as.matrix(dist(dist.border))
    dist.border.mat[mask.mat] = NA
    split.costfun = dist.mat / (pmin(dist.border.mat, tail(dist.border,1)-dist.border.mat))^split.costexp
  } else {
    dist.border.mat = abs(matrix(rep(1:N,N),nrow=N,ncol=N) - matrix(rep(1:N,each=N),nrow=N,ncol=N))
    dist.border.mat[mask.mat] = NA
    split.costfun = dist.mat / (pmin(dist.border.mat, N-dist.border.mat))^split.costexp
  }
  
  split.cleanup.keeptripoints = FALSE
  if (split.cleanup == "keeptripoints") {
    split.cleanup.keeptripoints = TRUE
    split.cleanup = "none"
  }
  
  if (!any(!is.na(split.costfun))) {
    if (split.cleanup == "independent") {
      which.keep = (angles != pi)
      lon = lon[which.keep]
      lat = lat[which.keep]
    }
    return(list(list(lon = lon, lat = lat)))
  }
  split.inds = which(split.costfun == min(split.costfun,na.rm=TRUE), arr.ind = TRUE)[1,]
  split.inds = split.inds[order(split.inds)]
  ind1 = split.inds[1]
  ind2 = split.inds[2]
  split.dist = dist.mat[ind1,ind2]
  
  checkfail.maskN = max(floor((floor(checkfail.maskfrac * N) - 1) / 2), 0)
  
  repeat {
    
    inds.1 = (ind1+1):(ind2-1)
    
    inds.2 = NULL
    if (ind2 < N) {inds.2 = (ind2+1):N}
    if (ind1 > 1) {inds.2 = c(inds.2,1:(ind1-1))}
    
    if (split.dist == 0) {break}
    
    split.ang1 = sl.angle(c(lon.ext[c(ind1,ind1+1)],lon[ind2]),
                          c(lat.ext[c(ind1,ind1+1)],lat[ind2]),left=TRUE)
    split.ang2 = sl.angle(c(lon.ext[c(ind2,ind2+1)],lon[ind1]),
                          c(lat.ext[c(ind2,ind2+1)],lat[ind1]),left=TRUE)
    
    angles.OK = (split.ang1 >= 0 && split.ang1 <= angles[ind1] && split.ang2 >= 0 && split.ang2 <= angles[ind2])
    nocross = TRUE
    if (angles.OK && checkcross.output) {
      
      split.lon = lon[split.inds]
      split.lat = lat[split.inds]
      if (length(inds.1) > 1) {
        intersect.res = sl.intersect(split.lon, split.lat, lon[inds.1], lat[inds.1],
                                   dist1 = split.dist, dist2 = dist.border.pairwise[inds.1[-length(inds.1)]],
                                   return.on.firsthit = TRUE)
        nocross = !intersect.res$anylines.intersect
      }
      if (nocross && length(inds.2) > 1) {
        intersect.res = sl.intersect(split.lon, split.lat, lon[inds.2], lat[inds.2],
                                     dist1 = split.dist, dist2 = dist.border.pairwise[inds.2[-length(inds.2)]],
                                     return.on.firsthit = TRUE)
        nocross = !intersect.res$anylines.intersect
      }
      
    }
    
    if (angles.OK && nocross) {break}
    
    if (checkfail.maskN > 0) {
      ind1 = (((ind1-checkfail.maskN-1):(ind1+checkfail.maskN-1)) %% N) + 1
      ind2 = (((ind2-checkfail.maskN-1):(ind2+checkfail.maskN-1)) %% N) + 1
    }
    mask.mat[ind1,ind2] = TRUE
    mask.mat[ind2,ind1] = TRUE
    split.costfun[ind1,ind2] = NA
    split.costfun[ind2,ind1] = NA
    if (!any(!is.na(split.costfun))) {
      if (split.cleanup == "independent") {
        which.keep = (angles != pi)
        lon = lon[which.keep]
        lat = lat[which.keep]
      }
      return(list(list(lon = lon, lat = lat)))
    }
    split.inds = which(split.costfun == min(split.costfun,na.rm=TRUE), arr.ind = TRUE)[1,]
    split.inds = split.inds[order(split.inds)]
    ind1 = split.inds[1]
    ind2 = split.inds[2]
    split.dist = dist.mat[ind1,ind2]
    
  }
  
  if (is.function(split.maxstep)) {
    split.maxstep.val = split.maxstep(dist.border.pairwise)
    if (!split.maxstep.passfun) {split.maxstep = split.maxstep.val}
  } else {
    split.maxstep.val = split.maxstep
  }
  if (split.maxstep.val <= 0) {stop("maximum split step width derived from 'split.maxstep' must be positive")}
  np = 0
  split.insert = list(lon=NULL,lat=NULL)
  split.insert.inv = list(lon=NULL,lat=NULL)
  
  if (split.dist > 0) {
    
    if (split.dist > split.maxstep.val) {
      np = ceiling(split.dist/split.maxstep.val) - 1
      split.insert = sl.fillequidist(c(lon[ind2],lon[ind1]),c(lat[ind2],lat[ind1]),np=np,method="gc")
      split.insert.inv = list(lon=split.insert$lon[np:1],lat=split.insert$lat[np:1])
    }
    inds.1.ext = c(ind1,inds.1,ind2)
    inds.2.ext = c(ind2,inds.2,ind1)
    lon1 = c(lon[inds.1.ext], split.insert$lon)
    lat1 = c(lat[inds.1.ext], split.insert$lat)
    lon2 = c(lon[inds.2.ext], split.insert.inv$lon)
    lat2 = c(lat[inds.2.ext], split.insert.inv$lat)
    angles1 = c((angles[ind1]-split.ang1),angles[inds.1],split.ang2,rep(pi,np))
    angles2 = c((angles[ind2]-split.ang2),angles[inds.2],split.ang1,rep(pi,np))
    
  } else {
    
    inds.1.ext = c(ind1,inds.1)
    inds.2.ext = c(ind2,inds.2)
    lon1 = lon[inds.1.ext]
    lat1 = lat[inds.1.ext]
    lon2 = lon[inds.2.ext]
    lat2 = lat[inds.2.ext]
    
    N.1 = length(inds.1.ext)
    angles1 = c(sl.angle(lon1[c(N.1,1,2)],lat1[c(N.1,1,2)],left=TRUE),angles[inds.1])
    if (sum(angles1) > N.1*pi) {
      lon1 = lon1[N.1:1]
      lat1 = lat1[N.1:1]
      inds.1.ext = inds.1.ext[N.1:1]
      angles1 = 2*pi - angles1[N.1:1]
    }
    
    N.2 = length(inds.2.ext)
    angles2 = c(sl.angle(lon2[c(N.2,1,2)],lat2[c(N.2,1,2)],left=TRUE),angles[inds.2])
    if (sum(angles2) > N.2*pi) {
      lon2 = lon2[N.2:1]
      lat2 = lat2[N.2:1]
      inds.2.ext = inds.2.ext[N.2:1]
      angles2 = 2*pi - angles2[N.2:1]
    }
    
  }
  
  stop.maxpoints = max(stop.maxpoints, 3)
  
  continue.1 = TRUE
  if (stop.maxiter == 1) {continue.1 = FALSE}
  else if (length(lon1) <= stop.maxpoints) {continue.1 = FALSE}
  else if (stop.maxangle > 0 && !any(angles1 > stop.maxangle)) {continue.1 = FALSE}
  
  continue.2 = TRUE
  if (stop.maxiter == 1) {continue.2 = FALSE}
  else if (length(lon2) <= stop.maxpoints) {continue.2 = FALSE}
  else if (stop.maxangle > 0 && !any(angles2 > stop.maxangle)) {continue.2 = FALSE}
  
  if ((continue.1 || continue.2) && !is.null(split.insert$lon)) {
    #dist.splitVSsplit = sl.finddist(split.insert$lon,split.insert$lat,return.alldist = TRUE)$dist.all
    xyz.split = sl.lonlat2xyz(lon = split.insert$lon, lat = split.insert$lat)
    xyz.split = cbind(xyz.split$x,xyz.split$y,xyz.split$z)
    dist.splitVSsplit = 2*asin(as.matrix(dist(xyz.split))/2)
  }
  
  if (continue.1) {
    dist.mat.1 = dist.mat[inds.1.ext,inds.1.ext]
    mask.mat.1 = mask.mat[inds.1.ext,inds.1.ext]
    if (!is.null(split.insert$lon)) {
      dist.splitVS1 = sl.finddist(split.insert$lon, split.insert$lat, lon[inds.1.ext], lat[inds.1.ext], return.alldist=TRUE)$dist.all
      dist.mat.1 = cbind(rbind(dist.mat.1,dist.splitVS1),rbind(t(dist.splitVS1),dist.splitVSsplit))
      N.1 = length(inds.1.ext)
      mask.mat.1 = cbind(rbind(mask.mat.1,cbind(rep(TRUE,np),matrix(rep(FALSE,(N.1-2)*np),nrow=np),rep(TRUE,np))),
                         rbind(rep(TRUE,np),matrix(rep(FALSE,(N.1-2)*np),ncol=np),rep(TRUE,np),matrix(rep(TRUE,np^2),ncol=np)))
      mask.mat.1[1,N.1] = TRUE
      mask.mat.1[N.1,1] = TRUE
    }
    polygon.list.1 = sl.polygon.split(lon = lon1,
                                      lat = lat1,
                                      angles = angles1,
                                      dist.mat = dist.mat.1,
                                      mask.mat = mask.mat.1,
                                      split.costexp = split.costexp,
                                      split.costborderbygcdist = split.costborderbygcdist,
                                      split.maxstep = split.maxstep,
                                      split.maxstep.passfun = split.maxstep.passfun,
                                      split.cleanup = split.cleanup,
                                      checkcross.input = FALSE,
                                      checkcross.output = checkcross.output,
                                      checkfail.maskfrac = checkfail.maskfrac,
                                      stop.maxiter = stop.maxiter - 1,
                                      stop.maxpoints = stop.maxpoints,
                                      stop.maxangle = stop.maxangle)
  } else {
    if (split.cleanup == "independent") {
      which.keep = (angles1 != pi)
      lon1 = lon1[which.keep]
      lat1 = lat1[which.keep]
    }
    polygon.list.1 = list(list(lon = lon1, lat = lat1))
  }
  
  if (continue.2) {
    dist.mat.2 = dist.mat[inds.2.ext,inds.2.ext]
    mask.mat.2 = mask.mat[inds.2.ext,inds.2.ext]
    if (!is.null(split.insert.inv$lon)) {
      dist.splitinvVS2 = sl.finddist(split.insert.inv$lon, split.insert.inv$lat, lon[inds.2.ext], lat[inds.2.ext], return.alldist=TRUE)$dist.all
      dist.mat.2 = cbind(rbind(dist.mat.2,dist.splitinvVS2),rbind(t(dist.splitinvVS2),dist.splitVSsplit))
      N.2 = length(inds.2.ext)
      mask.mat.2 = cbind(rbind(mask.mat.2,cbind(rep(TRUE,np),matrix(rep(FALSE,(N.2-2)*np),nrow=np),rep(TRUE,np))),
                         rbind(rep(TRUE,np),matrix(rep(FALSE,(N.2-2)*np),ncol=np),rep(TRUE,np),matrix(rep(TRUE,np^2),ncol=np)))
      mask.mat.2[1,N.2] = TRUE
      mask.mat.2[N.2,1] = TRUE
    }
    polygon.list.2 = sl.polygon.split(lon = lon2,
                                      lat = lat2,
                                      angles = angles2,
                                      dist.mat = dist.mat.2,
                                      mask.mat = mask.mat.2,
                                      split.costexp = split.costexp,
                                      split.costborderbygcdist = split.costborderbygcdist,
                                      split.maxstep = split.maxstep,
                                      split.maxstep.passfun = split.maxstep.passfun,
                                      split.cleanup = split.cleanup,
                                      checkcross.input = FALSE,
                                      checkcross.output = checkcross.output,
                                      checkfail.maskfrac = checkfail.maskfrac,
                                      stop.maxiter = stop.maxiter - 1,
                                      stop.maxpoints = stop.maxpoints,
                                      stop.maxangle = stop.maxangle)
  } else {
    if (split.cleanup == "independent") {
      which.keep = (angles2 != pi)
      lon2 = lon2[which.keep]
      lat2 = lat2[which.keep]
    }
    polygon.list.2 = list(list(lon = lon2, lat = lat2))
  }

  return.list = c(polygon.list.1,polygon.list.2)
  
  if (split.cleanup.keeptripoints) {
    N.poly = length(return.list)
    len.poly = rep(0,N.poly)
    for (i in 1:N.poly) {len.poly[i] = length(return.list[[i]]$lon)}
    end.inds = cumsum(len.poly)
    start.inds = c(1,end.inds[1:(N.poly-1)]+1)
    N.points = sum(len.poly)
    lats.all = numeric(length = N.points)
    lons.all = numeric(length = N.points)
    for (i in 1:N.poly) {
      lats.all[start.inds[i]:end.inds[i]] = return.list[[i]]$lat
      lons.all[start.inds[i]:end.inds[i]] = return.list[[i]]$lon
    }
    for (i in 1:N.poly) {
      which.keep = rep(FALSE,len.poly[i])
      for (j in 1:len.poly[i]) {
        lo = return.list[[i]]$lon[j]
        la = return.list[[i]]$lat[j]
        which.keep[j] = (any(lo == lon & la == lat) || sum(lo == lons.all & la == lats.all) > 2)
      }
      if (any(!which.keep)) {
        return.list[[i]] = list(lon = return.list[[i]]$lon[which.keep], lat = return.list[[i]]$lat[which.keep])
      }
    }
    
  }
    
  return(return.list)
	
}
