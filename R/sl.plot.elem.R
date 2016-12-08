sl.plot.elem <-
function (plot.init.res,lon,lat,elem,border=TRUE,col.border="black",length.corr="none",border.colbar=sl.colbar.redgreyblue_256,border.colbar.breaks=NA,border.colbar.breaks.log=FALSE,fill=TRUE,col.fill="grey",cell_area=NA,lwd=1,lty=1) {
	
	if (fill) {
		Ne = nrow(elem)
		for (ne in 1:Ne) {
			sl.plot.polygon.qad(plot.init.res,lon[elem[ne,]],lat[elem[ne,]],col.fill=col.fill)
		}
	}
	
	if (border) {
		edges = sl.elem2linepairs(elem,concat=FALSE)
		Ne = nrow(edges)
		if (col.border == "length") {
			len = rep(NA,Ne)
			for (ne in 1:Ne) {
				len[ne] = sl.gc.dist(lon[edges[ne,]],lat[edges[ne,]],Rsphere=6371)
			}
			if (length.corr == "triag2quad") {len = sqrt(cos(pi/6)) * len}
			colbar.res = sl.num2colbar(len,border.colbar,border.colbar.breaks,border.colbar.breaks.log)
			col.ind = colbar.res$colour.index
		}
		cb = col.border
		for (ne in 1:Ne) {
			if (col.border == "length") {
				cb = border.colbar[[col.ind[ne]]]
			}
			sl.plot.lines(plot.init.res,lon[edges[ne,]],lat[edges[ne,]],col=cb,lwd=lwd,lty=lty)
		}
	}
	
	return(get0("colbar.res"))
	
}
