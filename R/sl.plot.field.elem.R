sl.plot.field.elem <-
function (plot.init.res,num,lon,lat,elem,fill=TRUE,col.fill="colbar",border=FALSE,col.border="colbar",colbar=sl.colbar.redgreyblue_256,colbar.breaks=NA,colbar.breaks.log=FALSE,border.lwd=0.01,border.lty=1) {
	
	Ne = nrow(elem)
	num.elem = rep(NA,Ne)
	lon.elem = matrix(nrow=Ne,ncol=3)
	lat.elem = matrix(nrow=Ne,ncol=3)
	for (i in 1:Ne) {
		num.elem[i] = mean(num[elem[i,]])
		lon.elem[i,] = lon[elem[i,]]
		lat.elem[i,] = lat[elem[i,]]
	}
	
	colbar.res = sl.plot.field(plot.init.res=plot.init.res,num=num.elem,lon.v=lon.elem,lat.v=lat.elem,fill=fill,col.fill=col.fill,border=border,col.border=col.border,colbar=colbar,colbar.breaks=colbar.breaks,colbar.breaks.log=colbar.breaks.log,border.lwd=border.lwd,border.lty=border.lty)
	
	return(get0("colbar.res"))
	
}
