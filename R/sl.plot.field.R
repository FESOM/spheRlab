sl.plot.field <-
function (plot.init.res,num,lon.v,lat.v,fill=TRUE,col.fill="colbar",border=FALSE,col.border="colbar",colbar=sl.colbar.redgreyblue_256,colbar.breaks=NA,colbar.breaks.log=FALSE,border.lwd=0.01,border.lty=1) {
	
	Npoly = nrow(lon.v)
	if (col.fill == "colbar" || col.border == "colbar") {
		colbar.res = sl.num2colbar(num,colbar,colbar.breaks,colbar.breaks.log)
		col.ind = colbar.res$colour.index
	}
	for (np in 1:Npoly) {
		cb.fill = col.fill
		cb.border = col.border
		if (col.fill == "colbar") {cb.fill = colbar[[col.ind[np]]]}
		if (col.border == "colbar") {cb.border = colbar[[col.ind[np]]]}
		sl.plot.polygon.qad(plot.init.res,lon.v[np,],lat.v[np,],fill=fill,col.fill=cb.fill,border=border,col.border=cb.border,border.lwd=border.lwd,border.lty=border.lty)
	}
	
	if (exists("colbar.res")) {
		return(colbar.res)
	} else {
		return(NULL)
	}
	
}
