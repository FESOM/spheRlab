sl.plot.lonlatlabels <-
function (plot.init.res,lat.lons=NULL,lat.lats=NULL,lat.offsetlatlon=c(0,0),lon.lons=NULL,lon.lats=NULL,lon.offsetlatlon=c(0,0),col="grey",cex=1) {
	
  if (!is.null(lat.lons) && !is.null(lat.lats)) {
    for (lo in lat.lons) {
      labels = paste0(lat.lats,"N")
      labels[lat.lats<0] = paste0(-lat.lats[lat.lats<0],"S")
      sl.plot.text(plot.init.res,labels=labels,lon=rep(lo,length(lat.lats))+lat.offsetlatlon[2],lat=lat.lats+lat.offsetlatlon[1],cex=cex,col=col)
    }
  }
  
  if (!is.null(lon.lons) && !is.null(lon.lats)) {
    for (lo in lon.lons) {
      labels = paste0(rep(lo,length(lon.lats)),"E")
      if (lo<0) {labels = paste0(rep(-lo,length(lon.lats)),"W")}
      #labels=paste0(lo,"E")
      #labels[lon.lats<0] = paste0(-lon.lats[lon.lats<0],"W")
      sl.plot.text(plot.init.res,labels=labels,lon=rep(lo,length(lon.lats))+lon.offsetlatlon[2],lat=lon.lats+lon.offsetlatlon[1],cex=cex,col=col)
    }
  }
	
}