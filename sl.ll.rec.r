sl.ll.rec <- function(lon,lat){

dim_lon <- dim(lon)
dim_lat <- dim(lat)

if (length(dim_lon)>=2 || length(dim_lon)>=2){
	error <- "ATTENTION: lon or lat are already on a 2D grids; this function is not useful"
	print(error)
	lon_m<-lon
	lat_m<-lat
	}
	

if (length(dim_lon)==1 || length(dim_lon)==1){
	lon_m <- matrix(lon, nrow=dim_lon, ncol=dim_lat)
	lat_m <- matrix(0, nrow=dim_lon, ncol=dim_lat)

	for (i in 1:dim_lat){
		for (j in 1:dim_lon){
			lat_m[j,i]=lat[i]
		}
	}
}

return(list(lon=lon_m,lat=lat_m))
}