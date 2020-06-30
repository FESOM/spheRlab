sl.points2ellipse <-
  function (lon = NULL, lat = NULL, prob.within = 0.95, npoints = 360, return.ellispecs=F, Rsphere=1) {
    
    require(mixtools)
    
    if (sum(!(is.na(lon) | is.na(lat))) < 2) {
      return(list(lon=rep(NA,npoints), lat=rep(NA,npoints)))
    }
    if (!return.ellispecs & Rsphere!= 1){"You gave 'Rsphere' as input parameter but it won't be used, 
      you have to set 'return.ellispecs' to TRUE as well. "}
    
    baryc = sl.barycenter(lon = lon, lat = lat)
    abg = sl.lonlatrot2abg(c(baryc$lon,baryc$lat,0))
    rot.res = sl.rot(lon, lat, alpha=abg[1], beta=abg[2], gamma=abg[3], return.xyz = TRUE)
    mat = matrix(c(rot.res$x,rot.res$y),ncol=2)
    
    elli = ellipse(mu = colMeans(mat), sigma = cov(mat), alpha = 1-prob.within, npoints = npoints, draw = FALSE)
    elli.xysqsum = elli[,1]^2 + elli[,2]^2
    if (any(elli.xysqsum > 1)) {
      stop("ellipse is too wide for back-projection from xy-plane to sphere, try smaller 'prob.within'")
    }
    elli.z = sqrt(1 - (elli.xysqsum))
    elli.lonlatrot = apply(matrix(c(elli[,1],elli[,2],elli.z),ncol=3),1,sl.xyz2lonlat)
    elli.lonlat = sl.rot(lon=elli.lonlatrot[1,], lat=elli.lonlatrot[2,],
                         alpha=abg[1], beta=abg[2], gamma=abg[3], invert=TRUE, return.xyz = FALSE)
    
    if(!return.ellispecs){
      return(list(lon=elli.lonlat$lon, lat=elli.lonlat$lat))
    } else {
      sig=cov(mat*Rsphere)
      es  = eigen(sig)
      csq = qchisq(prob.within, 2)
      a = sqrt(csq*max(es$values)); b = sqrt(csq*min(es$values))
      area = a * b * pi
      return(list(lon=elli.lonlat$lon, lat=elli.lonlat$lat, area=area, a=a, b=b))
    }
  }