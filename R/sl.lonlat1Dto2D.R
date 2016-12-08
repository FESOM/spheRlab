sl.lonlat1Dto2D <-
function (lon=NULL,lat=NULL,Nlon=NULL,Nlat=NULL,swap=FALSE) {
	
	if (!is.null(lon)) {
		if (length(sl.dim(lon)) != 1) {stop("'lon' must be a vector")}
	}
	if (!is.null(lat)) {
		if (length(sl.dim(lat)) != 1) {stop("'lat' must be a vector")}
	}
	
	if (!is.null(lon)) {
		if (is.null(lat)) {
			if (is.null(Nlat)) {stop("to inflate 'lon', either 'lat' or 'Nlat' must be given")}
		} else {
			if (!is.null(Nlat)) {warning("deriving 'Nlat' from 'lat', ignoring 'Nlat'")}
			Nlat = length(lat)
		}
		if (!swap) {
			lon.out = matrix(rep(lon,Nlat),byrow=TRUE,nrow=Nlat)
		} else {
			lon.out = matrix(rep(lon,Nlat),byrow=FALSE,ncol=Nlat)
		}
	} else {
		lon.out = lon
	}
	
	if (!is.null(lat)) {
		if (is.null(lon)) {
			if (is.null(Nlon)) {stop("to inflate 'lat', either 'lon' or 'Nlon' must be given")}
		} else {
			if (!is.null(Nlon)) {warning("deriving 'Nlon' from 'lon', ignoring 'Nlon'")}
			Nlon = length(lon)
		}
		if (!swap) {
			lat.out = matrix(rep(lat,Nlon),byrow=FALSE,ncol=Nlon)
		} else {
			lat.out = matrix(rep(lat,Nlon),byrow=TRUE,nrow=Nlon)
		}
	} else {
		lat.out = lat
	}
	
	return(list(lon=lon.out,lat=lat.out))
	
}
