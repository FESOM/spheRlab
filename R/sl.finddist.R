sl.finddist <-
function(lon1,lat1,lon2=NULL,lat2=NULL,fun=min,return.vectors=TRUE,return.alldist=FALSE,exclude.zero=FALSE,return.degree=FALSE,Rsphere=1,reduce.memory=FALSE) {
  
  M = length(lon1)
  if (is.null(lon2)) {
    lon2 = lon1
    lat2 = lat1
  }
  N = length(lon2)
  
  if (!reduce.memory) {alldist = matrix(NA, nrow=M, ncol=N)}
  funres12.ind = numeric(M)
  funres12.dist = numeric(M)
  funres21.ind = numeric(N)
  funres21.dist = numeric(N)
  
  if (return.degree) {
    fac = 360 / 2 / pi / Rsphere
  } else {
    fac = 1 / Rsphere
  }
  
	for (m in 1:M) {
	  dists = sl.gc.dist(c(lon1[m],lon2), c(lat1[m],lat2), sequential = FALSE) * fac
	  dists[lon2==lon1[m] & lat2==lat1[m]] = 0
	  if (exclude.zero) {dists[dists==0] = NA}
	  if (!reduce.memory) {alldist[m,] = dists}
	  funres12.dist[m] = fun(dists,na.rm=TRUE)
	  funres12.ind[m] = match(funres12.dist[m],dists)
	}
  
  if (!reduce.memory) {
    for (n in 1:N) {
      funres21.dist[n] = fun(alldist[,n],na.rm=TRUE)
      funres21.ind[n] = match(funres21.dist[n],alldist[,n])
    }
  } else {
    for (n in 1:N) {
      dists = sl.gc.dist(c(lon2[n],lon1), c(lat2[n],lat1), sequential = FALSE) * fac
      dists[lon1==lon2[n] & lat1==lat2[n]] = 0
      if (exclude.zero) {dists[dists==0] = NA}
      funres21.dist[n] = fun(dists,na.rm=TRUE)
      funres21.ind[n] = match(funres21.dist[n],dists)
    }
  }
  
	if (!return.alldist || reduce.memory) {
	  alldist = NULL
	} else if (exclude.zero) {
	  alldist[is.na(alldist)] = 0
	}
  
  funres.dist = fun(funres21.dist)
  funres.ind = match(funres.dist,funres12.dist)
  funres.ind = c(funres.ind, funres12.ind[funres.ind])
  
  if (!return.vectors) {
    funres12.ind = NULL
    funres12.dist = NULL
    funres21.ind = NULL
    funres21.dist = NULL
  }
  
	return(list(ind=funres.ind, dist=funres.dist,
	            ind.12=funres12.ind, dist.12=funres12.dist,
	            ind.21=funres21.ind, dist.21=funres21.dist,
	            dist.all=alldist))
	
}
