sl.plot.naturalearth <-
function (plot.init.res=NULL,load.res=NULL,what="coastline",resolution="medium",poly.split=TRUE,
          lines.col="black",lwd=1,lty=1,fill.col="grey",fill.refine.boundary=TRUE,
          fill.refine.boundary.precision=1,polygon.borders=TRUE,polygon.borders.lwd=0.1,
          polygon.borders.col=fill.col,points.text=TRUE,points.text.col="black",points.text.cex=1,
          points.text.adj=NULL,points.text.pos=NULL,points.text.offset=0.5,points.text.vfont=NULL,
          points.text.font=NULL,points.points=FALSE,points.pch=20,points.col="black",points.cex=1,
          ignore.visibility=FALSE,naturalearth.dir=NULL,verbose=FALSE) {
	
	if (what[1] == "list") {
		return(sl.load.naturalearth(what="list",resolution=resolution))
	} else if (is.null(plot.init.res)) {
		stop("argument 'plot.init.res' missing (without default value)")
	}
	
  if (is.null(naturalearth.dir)) {
    if (file.exists("~/.spheRlab")) {
      source("~/.spheRlab",local=TRUE)
    }
    if (is.null(naturalearth.dir)) {
      naturalearth.dir = "~/naturalearthdata"
      warning(paste0("Assuming that 'naturalearth.dir' is '~/naturalearthdata'. You can specify a different default ",
                     "by setting this variable in a file '~/.spheRlab', or specify it directly with the corresponding ",
                     "function argument."))
    }
  }
  
	if (is.null(load.res)) {
		if (verbose) {print("Using 'sl.load.naturalearth()' to load Natural Earth data (as argument 'load.res' is not provided).")}
		load.res = sl.load.naturalearth(what=what,resolution=resolution,poly.split=poly.split,naturalearth.dir=naturalearth.dir,verbose=verbose)
	}
	
	N = length(load.res)
	lines.col = rep(lines.col,ceiling(N/length(lines.col)))[1:N]
	lwd = rep(lwd,ceiling(N/length(lwd)))[1:N]
	lty = rep(lty,ceiling(N/length(lty)))[1:N]
	fill.col = rep(fill.col,ceiling(N/length(fill.col)))[1:N]
	fill.refine.boundary = rep(fill.refine.boundary,ceiling(N/length(fill.refine.boundary)))[1:N]
	fill.refine.boundary.precision = rep(fill.refine.boundary.precision,ceiling(N/length(fill.refine.boundary.precision)))[1:N]
	polygon.borders = rep(polygon.borders,ceiling(N/length(polygon.borders)))[1:N]
	polygon.borders.lwd = rep(polygon.borders.lwd,ceiling(N/length(polygon.borders.lwd)))[1:N]
	points.text = rep(points.text,ceiling(N/length(points.text)))[1:N]
	points.text.col = rep(points.text.col,ceiling(N/length(points.text.col)))[1:N]
	points.text.cex = rep(points.text.cex,ceiling(N/length(points.text.cex)))[1:N]
	points.text.pos = rep(points.text.pos,ceiling(N/length(points.text.pos)))[1:N]
	points.text.offset = rep(points.text.offset,ceiling(N/length(points.text.offset)))[1:N]
	points.points = rep(points.points,ceiling(N/length(points.points)))[1:N]
	points.pch = rep(points.pch,ceiling(N/length(points.pch)))[1:N]
	points.col = rep(points.col,ceiling(N/length(points.col)))[1:N]
	points.cex = rep(points.cex,ceiling(N/length(points.cex)))[1:N]
	ignore.visibility = rep(ignore.visibility,ceiling(N/length(ignore.visibility)))[1:N]
	
	for (k in 1:N) {
		
		if (load.res[[k]]$type == "SpatialLines") {
			for (i in 1:length(load.res[[k]]$data)) {
				sl.plot.lines(plot.init.res=plot.init.res,lon=load.res[[k]]$data[[i]]$lon,lat=load.res[[k]]$data[[i]]$lat,
				              col=lines.col[k],lwd=lwd[k],lty=lty[k],ignore.visibility=ignore.visibility[k])
			}
		} else if (load.res[[k]]$type == "SpatialPolygons") {
			if (!is.na(fill.col[k])) {fill = TRUE} else {fill = FALSE}
			for (i in 1:length(load.res[[k]]$data)) {
				sl.plot.polygon(plot.init.res=plot.init.res,lon=load.res[[k]]$data[[i]]$lon,lat=load.res[[k]]$data[[i]]$lat,fill=fill,
				                col.fill=fill.col[k],border=polygon.borders[k],col.border=fill.col[k],border.lwd=polygon.borders.lwd[k],border.lty=lty[k],
				                ignore.visibility=ignore.visibility[k],remove.identical.neighbours=FALSE,refine.boundary=fill.refine.boundary[k],
				                refine.boundary.precision=fill.refine.boundary.precision[k])
			}
		} else if (load.res[[k]]$type == "SpatialPoints") {
			for (i in 1:length(load.res[[k]]$data)) {
				if (points.points[k]) {
					sl.plot.points(plot.init.res=plot.init.res,lon=load.res[[k]]$data[[i]]$lon,lat=load.res[[k]]$data[[i]]$lat,col=points.col[k],pch=points.pch[k],
					               cex=points.cex[k],ignore.visibility=ignore.visibility[k])
				}
				if (points.text[k]) {
					sl.plot.text(plot.init.res=plot.init.res,lon=load.res[[k]]$data[[i]]$lon,lat=load.res[[k]]$data[[i]]$lat,labels=load.res[[k]]$data[[i]]$name,
					             col=points.text.col[k],cex=points.text.cex[k],ignore.visibility=ignore.visibility[k],adj=points.text.adj,pos=points.text.pos[k],
					             offset=points.text.offset[k],vfont=points.text.vfont,font=points.text.font)
				}
			}
		}
		
	}
		
}
