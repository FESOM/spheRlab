sl.gc.dist <-
  function(lon=NULL,lat=NULL,x=NULL,y=NULL,z=NULL,Rsphere=1,sequential=TRUE,distmat=FALSE,distmat.split.index=NULL,byrot=FALSE) {
    
    if (!byrot) {
      if (is.null(x)) {
        if (is.null(lon)) {stop("Either 'lon,lat' or 'x,y,z' must be provided")}
        lon = pi / 180 * lon
        lat = pi / 180 * lat
        singlevec = (is.vector(lon) || length(dim(lon)) == 1)
        if (singlevec) {
          xyz.mat = cbind(cos(lat)*cos(lon),cos(lat)*sin(lon),sin(lat))
        } else {
          if (distmat) {stop("'distmat=TRUE' works only with a single sequence of points as input")}
          xyz.mat = cbind(cos(lat[,1])*cos(lon[,1]),cos(lat[,1])*sin(lon[,1]),sin(lat[,1]))
          xyz.mat.2 = cbind(cos(lat[,2])*cos(lon[,2]),cos(lat[,2])*sin(lon[,2]),sin(lat[,2]))
        }
      } else {
        singlevec = (is.vector(x) || length(dim(x)) == 1)
        if (singlevec) {
          xyz.mat = cbind(x,y,z)
        } else {
          if (distmat) {stop("'distmat=TRUE' works only with a single sequence of points as input")}
          xyz.mat = cbind(x[,1],y[,1],z[,1])
          xyz.mat.2 = cbind(x[,2],y[,2],z[,2])
        }
      }
      L = nrow(xyz.mat) - 1
    } else {
      if (is.null(lon)) {
        if (is.null(x)) {stop("Either 'lon,lat' or 'x,y,z' must be provided")}
        lola = sl.xyz2lonlat(x=x,y=y,z=z)
        lon = lola$lon
        lat = lola$lat
      }
      if (distmat) {stop("'distmat=TRUE' works only with 'byrot=FALSE'")}
      singlevec = (is.vector(lon) || length(dim(lon)) == 1)
      if (singlevec) {
        L = length(lon) - 1
      } else {
        L = nrow(lon)
      }
    }
    
    if (!distmat) {
      
      if (singlevec) {
        
        if (sequential) {
          
          if (byrot) {
            pointdist = rep(NA,L)
            for (l in 1:L) {
              alpha = lon[l] + 90
              beta = 90 - lat[l]
              gamma = 0
              rot.res = sl.rot(lon[l+1],lat[l+1],alpha,beta,gamma)
              pointdist[l] = (90 - rot.res$lat) * pi * Rsphere / 180
            }
          } else {
            if (L==1) {
              pointdist = (2*Rsphere)*asin(sqrt(sum((xyz.mat[2,]-xyz.mat[1,])^2))/2)
            } else {
              pointdist = (2*Rsphere)*asin(sqrt(rowSums((xyz.mat[2:(L+1),]-xyz.mat[1:L,])^2))/2)
            }
          }
          
        } else {
          
          if (byrot) {
            alpha = lon[1] + 90
            beta = 90 - lat[1]
            gamma = 0
            rot.res = sl.rot(lon[2:(L+1)],lat[2:(L+1)],alpha,beta,gamma)
            pointdist = (90 - rot.res$lat) * pi * Rsphere / 180
          } else {
            if (L==1) {
              pointdist = (2*Rsphere)*asin(sqrt(sum((xyz.mat[2,]-xyz.mat[1,])^2))/2)
            } else {
              pointdist = (2*Rsphere)*asin(sqrt(rowSums((xyz.mat[2:(L+1),]-
                                                         matrix(xyz.mat[1,],ncol=3,nrow=L,byrow=TRUE))^2))/2)
            }
          }
          
        }
        
      } else {
        
        if (byrot) {
          if (ncol(lon) != 2) {stop("With 'byrot=TRUE', if 'lon' and 'lat' are no vectors,
	                              they must be 2-column matrices or data frames")}
          pointdist = rep(NA,L)
          for (l in 1:L) {
            alpha = lon[l,1] + 90
            beta = 90 - lat[l,1]
            gamma = 0
            rot.res = sl.rot(lon[l,2],lat[l,2],alpha,beta,gamma)
            pointdist[l] = (90 - rot.res$lat) * pi * Rsphere / 180
          }
        } else {
          if (L==1) {
            pointdist = (2*Rsphere)*asin(sqrt(sum((xyz.mat-xyz.mat.2)^2))/2)
          } else {
            pointdist = (2*Rsphere)*asin(sqrt(rowSums((xyz.mat-xyz.mat.2)^2))/2)
          }
        }
        
      }
    
    } else {
      
      pointdist = as.matrix((2*Rsphere)*asin(dist(xyz.mat)/2))
      
    }
    
    return(pointdist)
    
  }
