sl.tracer.generate.testcase <- function(filepath = "~/"){
  require(zeallot)
  Uana <- function(x0, y0, c0=10, L0=4.5){
    A    = sqrt(x0^2 + y0^2)
    w    = c0/L0^2 * exp(-A^2/(2*L0^2))
    phi0 = atan2(x0,y0)
    
    u    = array(dim=c(2,length(x0)))
    u[1,] =   w*A*cos(phi0)
    u[2,] =  -w*A*sin(phi0)
    return(u)
  }
  
  ## generate grid and neighborhood information
  xlim = c(-10,10); ylim = c(-10,10); dx = dy = 1
  xgr = seq(xlim[1], xlim[2], dx); ygr = seq(ylim[1], ylim[2], dy)
  grid = sl.grid.curvilin2unstr(lon = xgr, lat = ygr)
  
  res <- sl.findneighbours(grid$elem, verbose = F)
  i.neighs <- lapply(1:length(grid$elem[,1]), function(ele) {
    dd <- unique(as.vector(res$neighbour.elems[grid$elem[ele,],]))
    if(any(is.na(dd))) dd = dd[-which(is.na(dd))]
    return(sort(dd))} )
  
  ## generate velocity field
  N.nodes = length(grid$lon); N.time  = 365
  U    = array(dim=c(2, N.nodes, N.time))
  for (t in 1:N.time) U[,,t] = Uana(grid$lon,grid$lat)
  
  ## generate initial conditions and store elements that contain them
  x.ini  = rbind(seq(0.5,5.0,0.5), rep(0,10)); N.ini = length(x.ini[1,])
  
  tri.cont.ini = rep(NA,N.ini)
  for(tar in 1:N.ini){
    xc = x.ini[,tar]
    c(mu, i.tri, i.pat) %<-% sl.tracer.get.triangle.of.target(triangles = grid$elem, points = rbind(grid$lon, grid$lat), target = xc)
    tri.cont.ini[tar]=i.tri
  }
  save(U, x.ini, grid, i.neighs, tri.cont.ini, file = file.path(filepath, "sl.tracer.example.data.RData"))
}