sl.lonlat.identical <-
function (lon1,lat1,lon2,lat2,recycle=FALSE,tolerance=0) {
	
	while(max(lon1) > 180) {
		lon1[lon1>180] = lon1[lon1>180] - 360
	}
	while(min(lon1) <= -180) {
		lon1[lon1<=180] = lon1[lon1<=180] + 360
	}
	while(max(lon2) > 180) {
		lon2[lon2>180] = lon2[lon2>180] - 360
	}
	while(min(lon2) <= -180) {
		lon2[lon2<=180] = lon2[lon2<=180] + 360
	}
	
	Llon1 = length(lon1)
	Llat1 = length(lat1)
	Llon2 = length(lon2)
	Llat2 = length(lat2)
	Lc = c(Llon1,Llat1,Llon2,Llat2)
	L = max(Lc)
	if (any(Lc < L & Lc != 1)) {
		if (recycle) {
			if (Llon1 < L && Llon1 > 1) {
				lon1 = rep(lon1,ceiling(L/Lc[1]))[1:L]
			}
			if (Llat1 < L && Llat1 > 1) {
				lat1 = rep(lat1,ceiling(L/Lc[2]))[1:L]
			}
			if (Llon2 < L && Llon2 > 1) {
				lon2 = rep(lon2,ceiling(L/Lc[3]))[1:L]
			}
			if (Llat2 < L && Llat2 > 1) {
				lat2 = rep(lat2,ceiling(L/Lc[4]))[1:L]
			}
		} else {
			stop("input sizes do not match, consider setting 'recycle=TRUE'")
		}
	}
	
	return(abs(lat1-lat2)<=tolerance & (abs(lon1-lon2)<=tolerance | abs(lat1+90)<=tolerance | abs(lat1-90)<=tolerance))
	
}
