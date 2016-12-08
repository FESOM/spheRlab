sl.plot.3D.init <-
function(file.name="~/sl.plot.pdf",projection="polar",width=30,lonlatrot.left=c(10,0,0),lonlatrot.right=c(-10,0,0),gap.frac=.1) {
	
	if (projection != "polar") {stop("projections other than 'polar' not yet implemented!")}
	
	pir.tmp = sl.plot.init(projection="polar",polar.lonlatrot=lonlatrot.left,polar.latbound=0,do.init=FALSE)
	xwidth = pir.tmp$xlim[2] - pir.tmp$xlim[1]
	xshift = xwidth / 2 * (1+gap.frac)
	xlim = c(pir.tmp$xlim[1]-xshift,pir.tmp$xlim[2]+xshift)
	ylim = pir.tmp$ylim
	height = width * (ylim[2]-ylim[1]) / (xlim[2]-xlim[1])
	
	pir.list = list()
	pir.list[[1]] = sl.plot.init(projection="polar",polar.lonlatrot=lonlatrot.left,polar.latbound=0,do.init=FALSE,xshift=-xshift,yshift=0)
	pir.list[[2]] = sl.plot.init(projection="polar",polar.lonlatrot=lonlatrot.right,polar.latbound=0,do.init=FALSE,xshift=xshift,yshift=0)
	
	pir.list$projection = "3D"
	pdf(file.name,width=width,height=height)
	par(mar=rep(0,4))
	plot(x=NULL,xlim=xlim,ylim=ylim,xlab="",ylab="",main="",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n",bg="white")
	return(pir.list)
	
}
