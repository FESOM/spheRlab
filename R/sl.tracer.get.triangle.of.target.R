sl.tracer.get.triangle.of.target <- function (triangles, points, target, cart_geo = "geo", patch.indices = NULL) {
  require(zeallot)
  # calculate barycentric coordinates
  J = length(points[1,])
  if (is.null(dim(triangles))){L=1} else {L = length(triangles[,1])}
  mu = matrix(nrow = 3, ncol = L) 
  
  if(L!=1){
    c(x1, x2, x3) %<-% sl.tracer.get.nodecoords.of.triangle(triangles=triangles, points=points)
    for (k in 1:length(x1[1,])) {
      if(cart_geo == "geo"){
        p1c = sl.lonlat2xyz(x1[,k])
        p2c = sl.lonlat2xyz(x2[,k])
        p3c = sl.lonlat2xyz(x3[,k])
        xc  = sl.lonlat2xyz(target)
      } else if (cart_geo == "cart"){
        if (length(x1[,1])==2){
          p1c = c(x1[,k],1)
          p2c = c(x2[,k],1)
          p3c = c(x3[,k],1)
          xc =  c(target,1)
        } else {
          p1c = x1[,k]; p2c = x2[,k]; p3c = x3[,k]; xc = target
        }
      } else {warning("cart_geo must either be 'cart' or 'geo'")}
      
      denom = 1/det(cbind(p1c,p2c,p3c))
      
      mu[1,k] = denom*det(cbind(xc,p2c,p3c))
      mu[2,k] = denom*det(cbind(p1c,xc,p3c))
      mu[3,k] = denom*det(cbind(p1c,p2c,xc))
    }
  } else {
    c(x1, x2, x3) %<-% sl.tracer.get.nodecoords.of.triangle(tri=triangles, points=points)
    if (cart_geo=="geo"){
      p1c = sl.lonlat2xyz(x1)
      p2c = sl.lonlat2xyz(x2)
      p3c = sl.lonlat2xyz(x3)
      xc  = sl.lonlat2xyz(target)
    } else if (cart_geo == "cart"){
      if (length(x1)==2){
        p1c = c(x1,1)
        p2c = c(x2,1)
        p3c = c(x3,1)
        xc =  c(target,1)
      } else {
        p1c = x1; p2c = x2; p3c = x3; xc = target
      }
    } else {warning("cart_geo must either be 'cart' or 'geo'")}
    denom = 1/det(cbind(p1c,p2c,p3c))
    
    mu[1,1] = denom*det(cbind(xc,p2c,p3c))
    mu[2,1] = denom*det(cbind(p1c,xc,p3c))
    mu[3,1] = denom*det(cbind(p1c,p2c,xc))
  }
  boo.tri <- apply(mu, 2, function(col) all(col >= -1e-8)) 
  
  if (!is.na(boo.tri[1]) & sum(boo.tri)==1) {
    ind.tri = which(boo.tri)
  } else if (!is.na(boo.tri[1]) & sum(boo.tri)>1) { 
    ind.tri = which(boo.tri)[1]} else {
      ind.tri = NA
    }
  
  patch.ind = ind.tri
  if(!is.null(patch.indices) & !is.na(ind.tri)){
    ind.tri = patch.indices[ind.tri]
  }
  return(list(mu, ind.tri, patch.ind))
}

