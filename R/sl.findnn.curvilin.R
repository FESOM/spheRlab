sl.findnn.curvilin <- function(lon=NULL,lat=NULL,x=NULL,y=NULL,z=NULL,lon.grid=NULL,lat.grid=NULL,x.grid=NULL,y.grid=NULL,z.grid=NULL,bruteforce=FALSE,Rsphere=1) {
  
  if (is.null(x)) {
    if (is.null(lon)) {stop("Either 'lon,lat' or 'x,y,z' must be provided")}
    if (is.null(lon.grid)) {stop("If the point(s) is (are) provided as lon-lat, the grid must be, too.")}
    M = nrow(lon.grid)
    N = ncol(lon.grid)
    xyz = sl.lonlat2xyz(lon = lon, lat = lat)
    x = xyz$x
    y = xyz$y
    z = xyz$z
  } else {
    if (is.null(x.grid)) {stop("If the point(s) is (are) provided in cartesian coordinates (x,...), the grid must be, too.")}
    M = nrow(x.grid)
    N = ncol(x.grid)
  }
  
  Np = length(x)
  nn.m.vec = integer(Np)
  nn.n.vec = integer(Np)
  dist.nn.vec = double(Np)
  
  for (ip in 1:Np) {
    
    if (bruteforce) {
      m.inds = 1:M
      n.inds = 1:N
    } else {
      m.step = (M - 1) / 4
      n.step = (N - 1) / 4
      m.inds = unique(round(seq(0,4) * m.step + 1))
      n.inds = unique(round(seq(0,4) * n.step + 1))
    }
    
    repeat {
      
      if (is.null(x.grid)) {
        xyz.sub = sl.lonlat2xyz(lon = lon.grid[m.inds,n.inds], lat = lat.grid[m.inds,n.inds])
        x.df = data.frame(x1 = x[ip], x2 = as.vector(xyz.sub$x))
        y.df = data.frame(y1 = y[ip], y2 = as.vector(xyz.sub$y))
        z.df = data.frame(z1 = z[ip], z2 = as.vector(xyz.sub$z))
      } else {
        x.df = data.frame(x1 = x[ip], x2 = as.vector(x.grid[m.inds,n.inds]))
        y.df = data.frame(y1 = y[ip], y2 = as.vector(y.grid[m.inds,n.inds]))
        z.df = data.frame(z1 = z[ip], z2 = as.vector(z.grid[m.inds,n.inds]))
      }
      
      dist.mat = matrix(sl.gc.dist(x=x.df,y=y.df,z=z.df),ncol=length(n.inds))
      dist.nn = min(dist.mat)
      nn.inds.sub = which(dist.mat == dist.nn, arr.ind = TRUE)[1,]
      nn.m = m.inds[nn.inds.sub[1]]
      nn.n = n.inds[nn.inds.sub[2]]
      
      done = TRUE
      if (max(diff(m.inds)) > 1) {
        done = FALSE
        m.step = m.step / 2
        m.inds = nn.m + unique(round(seq(-2,2) * m.step))
        m.inds = m.inds[m.inds > 0 & m.inds <= M]
      }
      if (max(diff(n.inds)) > 1) {
        done = FALSE
        n.step = n.step / 2
        n.inds = nn.n + unique(round(seq(-2,2) * n.step))
        n.inds = n.inds[n.inds > 0 & n.inds <= N]
      }
      
      if (done) {break}
      
    }
    
    nn.m.vec[ip] = nn.m
    nn.n.vec[ip] = nn.n
    dist.nn.vec[ip] = dist.nn
    
  }
  
  return(list(nn.index=data.frame(row=nn.m.vec,col=nn.n.vec),nn.dist=dist.nn.vec*Rsphere))
  
}