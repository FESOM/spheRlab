sl.curvilin2unstr <-
function (vals=NULL,byrow=TRUE,convert.grid=FALSE,lon.c=NULL,lat.c=NULL,lon.i=NULL,lat.i=NULL) {
	
	stop("not yet implemented")
	
	if (convert.grid) {
		
		#determine number of longitudes and latitudes
		#UNFINISHED!!!
		
		if (is.null(lon.i) || is.null(lat.i)) {
			
			if (length(sl.dim(lon.c)) == 1) {
				Nlon = length(lon.c)
				lon.i.2D = FALSE
			} else {
				lon.i.2D = TRUE
				if (byrow) {
					Nlon = sl.dim(lon.c)[2]
				} else {
					Nlon = sl.dim(lon.c)[1]
				}
			}
			if (length(sl.dim(lat.c)) == 1) {
				Nlat = length(lat.c)
				lat.c = matrix(rep(lat.c,Nlon),byrow=!byrow)
				lat.i.2D = FALSE
			} else {
				lat.i.2D = TRUE
				if (byrow) {
					Nlat = sl.dim(lat.c)[1]
				} else {
					Nlat = sl.dim(lat.c)[2]
				}
			}
			if (length(sl.dim(lon.c)) == 1) {
				lon.c = matrix(rep(lon.c,Nlat),byrow=byrow)
			}
			
			if (lon.i.2D) {
				stop("lon.i.2D case not yet implemented")
			} else {
				lon.i = rep(NA,Nlon+1)
				for (i in 2:Nlon) {
					lon.i[i] = sl.p2p(lon.c[i-1],0,lon.c[i],0,.5)$lon
				}
				lon.i[1] = sl.p2p(lon.c[2],0,lon.c[1],0,1.5)$lon
				lon.i[Nlon+1] = sl.p2p(lon.c[Nlon-1],0,lon.c[Nlon],0,1.5)$lon
				if (sl.gc.dist(c(lon.i[1],lon.c[1]),c(0,0)) > sl.gc.dist(c(lon.i[Nlon+1],lon.c[1]),c(0,0))) {
					lon.i[Nlon+1] = lon.i[1]
				}
				lon.i = rep(lon.i,Nlat+1)
				if (!byrow) {
					lon.i = as.vector(t(matrix(lon.i,nrow=Nlon+1)))
				}
			}
			
			if (lat.i.2D) {
				stop("lat.i.2D case not yet implemented")
			} else {
				lat.i = rep(NA,Nlat+1)
				lat.i[2:Nlat] = (lat.c[1:(Nlat-1)] + lat.c[2:Nlat]) / 2
				lat.i[1] = lat.c[1] - (lat.c[2]-lat.c[1])/2
				lat.i[1] = min(90,max(-90,lat.i[1]))
				lat.i[Nlat+1] = lat.c[Nlat] + (lat.c[Nlat]-lat.c[Nlat-1])/2
				lat.i[Nlat+1] = min(90,max(-90,lat.i[Nlat+1]))
				lat.i = rep(lat.i,Nlon+1)
				if (byrow) {
					lat.i = as.vector(t(matrix(lat.i,nrow=Nlat+1)))
				}
			}
			
		}
		
		if (length(sl.dim(lon.i)) == 1) {
			Nlon = length(lon.i) - 1
		} else {
			if (byrow) {
				Nlon = sl.dim(lon.i)[2] - 1
			} else {
				Nlon = sl.dim(lon.i)[1] - 1
			}
		}
		if (length(sl.dim(lat.i)) == 1) {
			Nlat = length(lat.i) - 1
			lat.i = matrix(rep(lat.i,Nlon+1),byrow=!byrow)
		} else {
			if (byrow) {
				Nlat = sl.dim(lat.i)[1] - 1
			} else {
				Nlat = sl.dim(lat.i)[2] - 1
			}
		}
		if (length(sl.dim(lon.i)) == 1) {
			lon.i = matrix(rep(lon.i,Nlat+1),byrow=byrow)
		}
		
		if (length(sl.dim(lon.c)) == 1) {
			lon.c = matrix(rep(lon.c,Nlat),byrow=byrow)
		}
		if (length(sl.dim(lat.c)) == 1) {
			lat.c = matrix(rep(lat.c,Nlon),byrow=!byrow)
		}
	
	}
	
	if (!is.null(vals)) {
		
		if (byrow) {
			vals = as.vector(t(vals))
		} else {
			vals = as.vector(vals)
		}
		
	}
	
	return(list(vals=vals,lon=lon.c,lat=lat.c,lon.vertices=lon.vertices,lat.vertices=lat.vertices))
	
}
