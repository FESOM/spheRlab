sl.grid.gen.ico <- function(Nseg = 1, edge.fillequidist.method = "xyz", cell_area = TRUE) {
  
  golden = (1+sqrt(5))/2
  v1 = c(0,1,golden)
  v1 = v1 / sqrt(sum(v1^2))
  v2 = c(0,-1,golden)
  v2 = v2 / sqrt(sum(v2^2))
  v3 = c(golden,0,1)
  v3 = v3 / sqrt(sum(v3^2))
  ang56 = sl.xyz.angle(v1,v1+v2+v3)
  ang66 = 2 * sl.xyz.angle(v1+v2,v1+v2+v3)
  lat3 = 90 - ang56
  lat1 = lat3 - ang66
  lat2 = -lat1 + ang56
  lons = c(0,seq(-4,5),0) * 36
  lats = c(90,rep(c(lat2,-lat2),5),-90)
  
  corners.mat = matrix(c(
    1,2,4,
    1,4,6,
    1,6,8,
    1,8,10,
    1,10,2,
    2,3,4,
    4,3,5,
    4,5,6,
    6,5,7,
    6,7,8,
    8,7,9,
    8,9,10,
    10,9,11,
    10,11,2,
    2,11,3,
    12,5,3,
    12,7,5,
    12,9,7,
    12,11,9,
    12,3,11
  ),ncol=3,byrow=TRUE)
  
  lon = NULL
  lat = NULL
  lon_bounds = NULL
  lat_bounds = NULL
  
  for (i in 1:20) {
    
    lon.e12 = lons[corners.mat[i,c(1,2)]]
    lat.e12 = lats[corners.mat[i,c(1,2)]]
    lon.e13 = lons[corners.mat[i,c(1,3)]]
    lat.e13 = lats[corners.mat[i,c(1,3)]]
    
    lon_bounds.mat = matrix(nrow=Nseg+1, ncol=Nseg+1)
    lat_bounds.mat = matrix(nrow=Nseg+1, ncol=Nseg+1)
    lon_bounds.mat[1,c(1,Nseg+1)] = lon.e12
    lat_bounds.mat[1,c(1,Nseg+1)] = lat.e12
    
    lon.mat = matrix(nrow=Nseg, ncol=Nseg)
    lat.mat = matrix(nrow=Nseg, ncol=Nseg)
    
    if (Nseg > 1) {
      lonlat.e12 = sl.fillequidist(lon = lon.e12, lat = lat.e12, np = Nseg-1, method = edge.fillequidist.method)
      lonlat.e13 = sl.fillequidist(lon = lon.e13, lat = lat.e13, np = Nseg-1, method = edge.fillequidist.method)
      lons.e12 = c(lon.e12[1],lonlat.e12$lon,lon.e12[2])
      lats.e12 = c(lat.e12[1],lonlat.e12$lat,lat.e12[2])
      lons.e13 = c(lon.e13[1],lonlat.e13$lon,lon.e13[2])
      lats.e13 = c(lat.e13[1],lonlat.e13$lat,lat.e13[2])
      lon_bounds.mat[1,2:Nseg] = lonlat.e12$lon
      lat_bounds.mat[1,2:Nseg] = lonlat.e12$lat
      lon_bounds.mat[2,2] = lons.e13[2]
      lat_bounds.mat[2,2] = lats.e13[2]
      for (j in 3:(Nseg+1)) {
        lonlat.e23 = sl.fillequidist(lon = c(lons.e12[j],lons.e13[j]),
                                     lat = c(lats.e12[j],lats.e13[j]),
                                     np = j-2, method = edge.fillequidist.method)
        lon_bounds.mat[2:(j-1),j] = lonlat.e23$lon
        lat_bounds.mat[2:(j-1),j] = lonlat.e23$lat
        lon_bounds.mat[j,j] = lons.e13[j]
        lat_bounds.mat[j,j] = lats.e13[j]
      }
    } else {
      
      lon_bounds.mat[2,2] = lon.e13[2]
      lat_bounds.mat[2,2] = lat.e13[2]
      
    }
    
    xyz_bounds.mat = sl.lonlat2xyz(lon = lon_bounds.mat, lat = lat_bounds.mat)
    x1.mat = xyz_bounds.mat$x[1:Nseg,1:Nseg] + xyz_bounds.mat$x[1:Nseg,2:(Nseg+1)] + xyz_bounds.mat$x[2:(Nseg+1),2:(Nseg+1)]
    y1.mat = xyz_bounds.mat$y[1:Nseg,1:Nseg] + xyz_bounds.mat$y[1:Nseg,2:(Nseg+1)] + xyz_bounds.mat$y[2:(Nseg+1),2:(Nseg+1)]
    z1.mat = xyz_bounds.mat$z[1:Nseg,1:Nseg] + xyz_bounds.mat$z[1:Nseg,2:(Nseg+1)] + xyz_bounds.mat$z[2:(Nseg+1),2:(Nseg+1)]
    x2.mat = xyz_bounds.mat$x[1:Nseg,1:Nseg] + xyz_bounds.mat$x[2:(Nseg+1),2:(Nseg+1)] + xyz_bounds.mat$x[2:(Nseg+1),1:Nseg]
    y2.mat = xyz_bounds.mat$y[1:Nseg,1:Nseg] + xyz_bounds.mat$y[2:(Nseg+1),2:(Nseg+1)] + xyz_bounds.mat$y[2:(Nseg+1),1:Nseg]
    z2.mat = xyz_bounds.mat$z[1:Nseg,1:Nseg] + xyz_bounds.mat$z[2:(Nseg+1),2:(Nseg+1)] + xyz_bounds.mat$z[2:(Nseg+1),1:Nseg]
    lonlat1 = sl.xyz2lonlat(x = x1.mat, y = y1.mat, z = z1.mat)
    lonlat2 = sl.xyz2lonlat(x = x2.mat, y = y2.mat, z = z2.mat)
    
    if (i == 1) {
      which1.mat = !is.na(lonlat1$lon)
      which2.mat = !is.na(lonlat2$lon)
    }
    
    lon = c(lon, lonlat1$lon[which1.mat], lonlat2$lon[which2.mat])
    lat = c(lat, lonlat1$lat[which1.mat], lonlat2$lat[which2.mat])
    
    lon_bounds.3col = matrix(nrow=Nseg^2,ncol=3)
    lon_bounds.3col[1:(Nseg*(Nseg+1)/2),1] = lon_bounds.mat[1:Nseg,1:Nseg][which1.mat]
    lon_bounds.3col[1:(Nseg*(Nseg+1)/2),2] = lon_bounds.mat[1:Nseg,2:(Nseg+1)][which1.mat]
    lon_bounds.3col[1:(Nseg*(Nseg+1)/2),3] = lon_bounds.mat[2:(Nseg+1),2:(Nseg+1)][which1.mat]
    if (Nseg > 1) {
      lon_bounds.3col[(Nseg*(Nseg+1)/2+1):(Nseg^2),1] = lon_bounds.mat[1:Nseg,1:Nseg][which2.mat]
      lon_bounds.3col[(Nseg*(Nseg+1)/2+1):(Nseg^2),2] = lon_bounds.mat[2:(Nseg+1),2:(Nseg+1)][which2.mat]
      lon_bounds.3col[(Nseg*(Nseg+1)/2+1):(Nseg^2),3] = lon_bounds.mat[2:(Nseg+1),1:Nseg][which2.mat]
    }
    lat_bounds.3col = matrix(nrow=Nseg^2,ncol=3)
    lat_bounds.3col[1:(Nseg*(Nseg+1)/2),1] = lat_bounds.mat[1:Nseg,1:Nseg][which1.mat]
    lat_bounds.3col[1:(Nseg*(Nseg+1)/2),2] = lat_bounds.mat[1:Nseg,2:(Nseg+1)][which1.mat]
    lat_bounds.3col[1:(Nseg*(Nseg+1)/2),3] = lat_bounds.mat[2:(Nseg+1),2:(Nseg+1)][which1.mat]
    if (Nseg > 1) {
      lat_bounds.3col[(Nseg*(Nseg+1)/2+1):(Nseg^2),1] = lat_bounds.mat[1:Nseg,1:Nseg][which2.mat]
      lat_bounds.3col[(Nseg*(Nseg+1)/2+1):(Nseg^2),2] = lat_bounds.mat[2:(Nseg+1),2:(Nseg+1)][which2.mat]
      lat_bounds.3col[(Nseg*(Nseg+1)/2+1):(Nseg^2),3] = lat_bounds.mat[2:(Nseg+1),1:Nseg][which2.mat]
    }
    lon_bounds = rbind(lon_bounds, lon_bounds.3col)
    lat_bounds = rbind(lat_bounds, lat_bounds.3col)
    
  }
  
  res.list = list(lon=lon, lat=lat, lon_bounds=lon_bounds, lat_bounds=lat_bounds)
  
  if (cell_area) {
    cellareas = rep(NA,Nseg^2)
    for (i in 1:(Nseg^2)) {
      cellareas[i] = sl.triag.area(lon = lon_bounds.3col[i,], lat = lat_bounds.3col[i,])
    }
    res.list$cellareas = rep(cellareas, 20)
  }

  return(res.list)
  
}
