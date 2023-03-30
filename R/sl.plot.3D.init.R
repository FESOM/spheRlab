sl.plot.3D.init <-
function(projection="polar",width=30,lonlatrot.left=c(10,0,0),lonlatrot.right=c(-10,0,0),gap.frac=.1,col.background=NULL,precision=1,device="pdf",do.init=TRUE,do.init.device=do.init,file.name=paste0("~/sl.plot.3D.",device),transform.function=NULL,mar=rep(0,4)) {
	
	if (projection != "polar") {stop("projections other than 'polar' not yet implemented!")}
	
	pir.tmp = sl.plot.init(projection="polar",polar.lonlatrot=lonlatrot.left,polar.latbound=0,do.init=FALSE)
	xwidth = pir.tmp$xlim[2] - pir.tmp$xlim[1]
	xshift = xwidth / 2 * (1+gap.frac)
	xlim = c(pir.tmp$xlim[1]-xshift,pir.tmp$xlim[2]+xshift)
	ylim = pir.tmp$ylim
	height = width * (ylim[2]-ylim[1]) / (xlim[2]-xlim[1])
	
	if (do.init.device) {
	  if (device %in% c("bmp","jpeg","png","tiff")) {width = width * 100}
	  dev.fun = match.fun(device,descend=FALSE)
	  dev.fun(file.name, width*(1+mar[2]+mar[4]), height*(1+mar[1]+mar[3]))
	}
	if (do.init) {
	  par(mar=rep(0,4))
	  plot.xlim = xlim
	  plot.ylim = ylim
	  plot.ylim[1] = plot.ylim[1] - mar[1] * diff(ylim)
	  plot.xlim[1] = plot.xlim[1] - mar[2] * diff(xlim)
	  plot.ylim[2] = plot.ylim[2] + mar[3] * diff(ylim)
	  plot.xlim[2] = plot.xlim[2] + mar[4] * diff(xlim)
	  plot(x=NULL,xlim=plot.xlim,ylim=plot.ylim,xlab="",ylab="",main="",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",bg="white")
	}
	
	pir.list = list()
	pir.list[[1]] = sl.plot.init(projection="polar",polar.lonlatrot=lonlatrot.left,polar.latbound=0,col.background=col.background,precision=precision,do.init=FALSE,xshift=-xshift,yshift=0)
	pir.list[[2]] = sl.plot.init(projection="polar",polar.lonlatrot=lonlatrot.right,polar.latbound=0,col.background=col.background,precision=precision,do.init=FALSE,xshift=xshift,yshift=0)
	
	pir.list$projection = "3D"
	pir.list$transform.function = transform.function
  
	return(pir.list)
	
}
