sl.lat1D.c2i <-
function (lat.c) {
	
	if (length(sl.dim(lat.c)) != 1) {stop("lat.c must be a vector")}
	
	Nlat = length(lat.c)
	lat.i = rep(NA,Nlat+1)
	lat.i[2:Nlat] = (lat.c[1:(Nlat-1)] + lat.c[2:Nlat]) / 2
	lat.i[1] = lat.c[1] - (lat.c[2]-lat.c[1])/2
	lat.i[1] = min(90,max(-90,lat.i[1]))
	lat.i[Nlat+1] = lat.c[Nlat] + (lat.c[Nlat]-lat.c[Nlat-1])/2
	lat.i[Nlat+1] = min(90,max(-90,lat.i[Nlat+1]))
	
	return(lat.i)
	
}
