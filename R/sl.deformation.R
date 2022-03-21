sl.deformation <- function(lon = NULL, lat = NULL, lonlat2xy.pole = NULL, x = NULL, y = NULL, u = NULL, v = NULL, dt = 1, Rsphere = 1, return.lonlat = FALSE, return.xyuv = FALSE) {
  
  if (is.null(lon)) {
    
    if (is.null(x) || is.null(y) || is.null(u) || is.null(v)) {
      stop("incomplete set of input arguments; either 'lon'-'lat' or 'x'-'y'-'u'-'v' must be provided")
    }
    warning("array geometry is not checked and, if required, adjusted when x,y,u,v are provided directly")
    if (return.lonlat) {
      warning("'lonlat' can not be returned when x,y,u,v are provided directly")
      return.lonlat = FALSE
    }
    
  } else {
    
    if (is.null(lat)) {
      stop("incomplete set of input arguments; either 'lon'-'lat' or 'x'-'y'-'u'-'v' must be provided")
    }
    if (!is.null(x) || !is.null(y) || !is.null(u) || !is.null(v)) {
      warning("ignoring arguments 'x', 'y', 'u', and 'v' as 'lon' and 'lat' are provided")
    }
    
    # check if initial array is arranged piecewise clockwise from the polygon barycenter
    if (length(dim(lon))==2) {
      midpoint = unlist(sl.barycenter(lon = lon[,1], lat = lat[,1]))
      lonx = c(lon[,1],lon[1,1])
      latx = c(lat[,1],lat[1,1])
      for (n in 1:dim(lon)[1]) {
        check.res = sl.checkposition(midpoint,c(lonx[n],latx[n]),c(lonx[n+1],latx[n+1]))
        if (check.res != -1) {stop("initial array is not arranged piecewise clockwise from the polygon barycenter")}
      }
    } else {
      for (j in 1:dim(lon)[1]) {
        midpoint = unlist(sl.barycenter(lon = lon[j,,1], lat = lat[j,,1]))
        lonx = c(lon[j,,1],lon[j,1,1])
        latx = c(lat[j,,1],lat[j,1,1])
        for (n in 1:dim(lon)[2]) {
          check.res = sl.checkposition(midpoint,c(lonx[n],latx[n]),c(lonx[n+1],latx[n+1]))
          if (check.res != -1) {stop(paste0("initial array ",j," is not arranged piecewise clockwise from the polygon barycenter"))}
        }
      }
    }
    
    if (is.null(lonlat2xy.pole)) {
      lonlat2xy.pole = unlist(sl.boundingcircle(lon = lon, lat = lat)[1:2])
      warning(paste0("Derived pole coordinates for conversion of lon,lat to x,y equatorial plane with sl.boundingcircle(); pole is at lon=",
                     lonlat2xy.pole[1],", lat=",lonlat2xy.pole[2]))
    }
    
    # convert lon,lat to x,y
    abg = sl.lonlatrot2abg(c(lonlat2xy.pole,0))
    xyz = sl.rot(lon = lon, lat = lat, alpha = abg[1], beta = abg[2], gamma = abg[3], return.xyz = TRUE)
    x = xyz$x * Rsphere
    y = xyz$y * Rsphere
    if (return.lonlat) {z = xyz$z * Rsphere}
    
  }
  
  if (is.null(u) || is.null(v)) {
    
    warning("Deriving velocities from locations at temporal mid-points; time dimension of output will be shorter by one element compared to the location input")
    
    # derive u,v from x,y
    if (length(dim(x))==2) {
      Nt = ncol(x) - 1
      u = (x[,2:(Nt+1)] - x[,1:Nt]) / dt
      v = (y[,2:(Nt+1)] - y[,1:Nt]) / dt
      x = (x[,2:(Nt+1)] + x[,1:Nt]) / 2
      y = (y[,2:(Nt+1)] + y[,1:Nt]) / 2
      if (return.lonlat) {z = (z[,2:(Nt+1)] + z[,1:Nt]) / 2}
    } else {
      Nt = dim(x)[3] - 1
      u = (x[,,2:(Nt+1)] - x[,,1:Nt]) / dt
      v = (y[,,2:(Nt+1)] - y[,,1:Nt]) / dt
      x = (x[,,2:(Nt+1)] + x[,,1:Nt]) / 2
      y = (y[,,2:(Nt+1)] + y[,,1:Nt]) / 2
      if (return.lonlat) {z = (z[,,2:(Nt+1)] + z[,,1:Nt]) / 2}
    }
      
  }
  
  if (length(dim(x))==2) {
    
    N = nrow(x) # number of buoys in perimeter
    
    # calculate area
    area = y[N,]*x[1,] - x[N,]*y[1,]
    for (n in 1:(N-1)){area = area + y[n,]*x[n+1,] - x[n,]*y[n+1,]}
    area = area * 0.5
    
    # calculate strain rate components
    dudx = (u[1,] + u[N,])*(y[1,] - y[N,])
    for (n in 1:(N-1)){dudx = dudx + (u[n+1,] + u[n,]) * (y[n+1,] - y[n, ])}
    dudx = -dudx / area / 2
    
    dudy = (u[1,] + u[N,])*(x[1,] - x[N,])
    for (n in 1:(N-1)){dudy = dudy + (u[n+1,] + u[n,]) * (x[n+1,] - x[n, ])}
    dudy = dudy / area / 2
    
    dvdx = (v[1,] + v[N,])*(y[1,] - y[N,])
    for (n in 1:(N-1)){dvdx = dvdx + (v[n+1,] + v[n,]) * (y[n+1,] - y[n, ])}
    dvdx = -dvdx / area / 2
    
    dvdy = (v[1,] + v[N,])*(x[1,] - x[N,])
    for (n in 1:(N-1)){dvdy = dvdy + (v[n+1,] + v[n,]) * (x[n+1,] - x[n, ])}
    dvdy = dvdy / area / 2
    
    D = dudx + dvdy   # divergence
    V = dvdx - dudy   # vorticity
    S = dudy + dvdx   # pure shear
    N = dudx - dvdy   # normal shear
    
  } else {
    
    N = dim(x)[2]
    
    # calculate area
    area = y[,N,]*x[,1,] - x[,N,]*y[,1,]
    for (n in 1:(N-1)){area = area + y[,n,]*x[,n+1,] - x[,n,]*y[,n+1,]}
    area = area * 0.5
    
    # calculate strain rate components
    dudx = (u[,1,] + u[,N,])*(y[,1,] - y[,N,])
    for (n in 1:(N-1)){dudx = dudx + (u[,n+1,] + u[,n,]) * (y[,n+1,] - y[,n, ])}
    dudx = -dudx / area / 2
    
    dudy = (u[,1,] + u[,N,])*(x[,1,] - x[,N,])
    for (n in 1:(N-1)){dudy = dudy + (u[,n+1,] + u[,n,]) * (x[,n+1,] - x[,n, ])}
    dudy = dudy / area / 2
    
    dvdx = (v[,1,] + v[,N,])*(y[,1,] - y[,N,])
    for (n in 1:(N-1)){dvdx = dvdx + (v[,n+1,] + v[,n,]) * (y[,n+1,] - y[,n, ])}
    dvdx = -dvdx / area / 2
    
    dvdy = (v[,1,] + v[,N,])*(x[,1,] - x[,N,])
    for (n in 1:(N-1)){dvdy = dvdy + (v[,n+1,] + v[,n,]) * (x[,n+1,] - x[,n, ])}
    dvdy = dvdy / area / 2
    
    D = dudx + dvdy   # divergence
    V = dvdx - dudy   # vorticity
    S = dudy + dvdx   # pure shear
    N = dudx - dvdy   # normal shear
    
  }
  
  res.list = list(divergence=D, vorticity=V, pure.shear=S, normal.shear=N,
                  total.def=sqrt(N^2+S^2+D^2), max.shear.strain=sqrt(N^2+S^2),
                  area=area, dudx=dudx, dudy=dudy, dvdx=dvdx, dvdy=dvdy, lonlat2xy.pole=lonlat2xy.pole)
  
  if (return.lonlat) {
    lonlat = sl.xyz2lonlat(x=x,y=y,z=z)
    lonlat = sl.rot(lon = lonlat$lon, lat = lonlat$lat, alpha = abg[1], beta = abg[2], gamma = abg[3], invert = TRUE)
    res.list = c(res.list, lonlat=lonlat)
  }
  
  if (return.xyuv) {
    res.list = c(res.list, xyuv=list(x=x,y=y,u=u,v=v))
  }
  
  return(res.list)

}