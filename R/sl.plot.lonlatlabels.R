sl.plot.lonlatlabels <-
function (plot.init.res,lat.lons=NULL,lat.lats=NULL,lat.offsetlatlon=c(0,0),lon.lons=NULL,lon.lats=NULL,lon.offsetlatlon=c(0,0),col="grey",labels.round.digits=NULL,cex=1) {
	
  if (!is.null(lat.lons) && !is.null(lat.lats)) {
    for (lo in lat.lons) {
      if (is.null(labels.round.digits)) {
        labels = paste0(lat.lats,"N")
        labels[lat.lats<0] = paste0(-lat.lats[lat.lats<0],"S")
      } else {
        labels = paste0(round(lat.lats,digits=labels.round.digits),"N")
        labels[lat.lats<0] = paste0(round(-lat.lats[lat.lats<0],digits=labels.round.digits),"S")
      }
      sl.plot.text(plot.init.res,labels=labels,lon=rep(lo,length(lat.lats))+lat.offsetlatlon[2],lat=lat.lats+lat.offsetlatlon[1],cex=cex,col=col)
    }
  }
  
  if (!is.null(lon.lons) && !is.null(lon.lats)) {
    for (lo in lon.lons) {
      if (is.null(labels.round.digits)) {
        labels = paste0(rep(lo,length(lon.lats)),"E")
        if (lo<0) {labels = paste0(rep(-lo,length(lon.lats)),"W")}
      } else {
        labels = paste0(rep(round(lo,digits=labels.round.digits),length(lon.lats)),"E")
        if (lo<0) {labels = paste0(rep(round(-lo,digits=labels.round.digits),length(lon.lats)),"W")}
      }
      sl.plot.text(plot.init.res,labels=labels,lon=rep(lo,length(lon.lats))+lon.offsetlatlon[2],lat=lon.lats+lon.offsetlatlon[1],cex=cex,col=col)
    }
  }
	
}
