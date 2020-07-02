sl.tracer.U.xinter <- function(x, U, triangles, points, linds, tri.cont=NA, cart_geo = "geo", patch.level = 1){
  flag=NA
  require(zeallot)
  if(is.na(tri.cont)){ # if we have to check whole grid
    c(mu, i.tri, i.pat) %<-% sl.tracer.get.triangle.of.target(
      triangles, points, x,
      cart_geo = cart_geo)
    if(is.na(i.tri)){flag = 1;  return(list(NA,NA,flag))}
  } else {
    # first, check old triangle
    c(mu, i.tri, i.pat) %<-% sl.tracer.get.triangle.of.target(
      triangles = triangles[tri.cont,], 
      points = points, 
      target = x,
      patch.indices = c(tri.cont),
      cart_geo = cart_geo)
    
    # if necessary, check patch
    if(is.na(i.tri)){
      patch <- linds[[tri.cont]]
      for (p in 1:patch.level){
        if (p > 1){patch = unique(unlist(linds[patch]))}
        c(mu, i.tri, i.pat) %<-% sl.tracer.get.triangle.of.target(
          triangles = triangles[patch,], 
          points = points, 
          target = x,
          patch.indices = patch,
          cart_geo = cart_geo)
        if (!is.na(i.tri)){break}
      }
    }
    
    # else: skip procedure for now, but do adaptive timestep later
    if(is.na(i.tri)){
      #warning(".. target left patch - flagging")
      flag = 1
      return(list(NA,NA,flag))
    }
  }
  
  tri = triangles[i.tri, 1:3]     # node indices of containing triangle
  
  # interpolate velocity
  mu[,i.pat] = mu[,i.pat]/sum(mu[,i.pat])  
  if (length(dim(U))==3){
    U_  = U[,tri,]
    U_interp =  mu[,i.pat][1] * U_[,1,] + mu[,i.pat][2] * U_[,2,] + mu[,i.pat][3] * U_[,3,]
  } else {
    U_ = U[,tri]
    U_interp =  mu[,i.pat][1] * U_[,1] + mu[,i.pat][2] * U_[,2] + mu[,i.pat][3] * U_[,3]
  }
  
  return(list(U_interp, i.tri, flag))
}