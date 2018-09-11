sl.plot.fld.curvilin <-
function (plot.init.res,vals,mask=NULL,lon.i,lat.i,border=TRUE,border.lwd=0.01,colbar=sl.colbar.blackwhite_256,colbar.breaks=NA,colbar.breaks.log=FALSE,na.col=NULL) {
	
	if (length(sl.dim(lon.i)) == 1) {
		if (length(sl.dim(lat.i)) == 1) {
			lonlat2D = sl.lonlat1Dto2D(lon.i,lat.i)
			lon.i = lonlat2D$lon
			lat.i = lonlat2D$lat
		} else {stop("'lon.i' and 'lat.i' must have same dimensionality")}
	} else {
		if (length(sl.dim(lat.i)) == 1) {stop("'lon.i' and 'lat.i' must have same dimensionality")}
	}
	
	if (!all(dim(vals)==dim(lon.i)-1) || !all(dim(vals)==dim(lat.i)-1)) {
		vals = t(vals)
		if (!all(dim(vals)==dim(lon.i)-1)) {
			stop("'vals', 'lon.i', and 'lat.i' have inconsistent sizes.")
		} else {if (!is.null(mask)) {mask = t(mask)}}
	}
	
	colbar.res = sl.num2colbar(vals,colbar,colbar.breaks,colbar.breaks.log)
	col.ind = colbar.res$colour.index
	
	if (is.null(mask)) {
		mask = matrix(rep(TRUE,prod(dim(vals))),nrow=dim(vals)[1])
	}
	
	M = nrow(vals)
	N = ncol(vals)
	
	for (m in 1:M) {
		for (n in 1:N) {
			if (mask[m,n]) {
			  if (is.na(vals[m,n])) {
			    if (is.null(na.col)) {next}
			  }
				p.lon = c(lon.i[m,n],lon.i[m+1,n],lon.i[m+1,n+1],lon.i[m,n+1])
				p.lat = c(lat.i[m,n],lat.i[m+1,n],lat.i[m+1,n+1],lat.i[m,n+1])
				if (is.na(vals[m,n])) {
				  sl.plot.polygon(plot.init.res,p.lon,p.lat,col.fill=na.col,border=border,border.lwd=border.lwd)
				} else {
				  sl.plot.polygon(plot.init.res,p.lon,p.lat,col.fill=colbar[[col.ind[m,n]]],border=border,border.lwd=border.lwd)
				}
			}
		}
	}
	
	return(colbar.res)
	
}
