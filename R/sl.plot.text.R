sl.plot.text <-
function (plot.init.res,lon,lat,labels=seq(1,length(lon)),col="black",ignore.visibility=FALSE,adj=NULL,pos=NULL,offset=0.5,vfont=NULL,cex=1,font=NULL) {
	
	projection = plot.init.res$projection
	
	if (projection == "platon" || projection == "3D") {
		
		npir = 1
		repeat {
			pir = plot.init.res[[npir]]
			if (!is.list(pir)) {return()}
			if (is.null(pir$projection)) {return()}
			sl.plot.text(pir,lon,lat,labels,col,ignore.visibility,adj,pos,offset,vfont,cex,font)
			npir = npir + 1
		}
		
	}
			
	vsr.res = sl.vis.shift.rot(plot.init.res,lon,lat)
	x = vsr.res$x
	y = vsr.res$y
	rot.lon = vsr.res$rot.lon
	rot.lat = vsr.res$rot.lat
	visible = vsr.res$visible
	
	if (ignore.visibility) {
		visible[] = TRUE
	}
	
	xshift = plot.init.res$xshift
	yshift = plot.init.res$yshift
	
	if (any(visible)) {
		text(x=x[visible]+xshift,y=y[visible]+yshift,labels=labels[visible],adj=adj,pos=pos,offset=offset,vfont=vfont,cex=cex,col=col,font=font)
	}
	
}
