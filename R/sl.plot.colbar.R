sl.plot.colbar <-
function (colbar,breaks,vertical=TRUE,labels.at=rep(TRUE,length(breaks)),labels=as.character(signif(breaks,3)),labels.side="bottom",labels.cex=1,ticks.length=1,ticks.mirrored=FALSE,ratio=.1,triag.ends=FALSE,file.name="~/sl.plot.colbar.pdf",width=6) {
	
	if (length(breaks) != length(colbar) - 1) {
		stop("'breaks' must be shorter by exactly one element compared to 'colbar'")
	}
	
	pdf(file.name,width=width,height=width)
	par(mar=rep(0,4))
	
	plot(NA,xlim=c(-0.5,0.5),ylim=c(-0.5,0.5),bty="n",xaxt="n",yaxt="n")
	if (vertical) {
		
		ymin = -0.3
		ymax = 0.3
		ystp = (ymax - ymin) / (length(breaks) + 1)
		xmax = (ymax - ymin) * ratio / 2
		xmin = -xmax
		
		ymi = ymin
		yma = ymi + ystp
		polygon(x=c(xmin,xmax,xmax,xmin),y=c(ymi,ymi,yma,yma),col=colbar[[1]],border=colbar[[1]],lwd=.01)
		for (i in 2:length(colbar)) {
			ymi = ymi + ystp
			yma = ymi + ystp
			polygon(x=c(xmin,xmax,xmax,xmin),y=c(ymi,ymi,yma,yma),col=colbar[[i]],border=colbar[[i]],lwd=.01)
			if (labels.at[i-1]) {
				text(x=1.25*xmax,y=ymi,labels=labels[i-1],pos=4,cex=labels.cex)
			}
		}
		
		if (!triag.ends) {
			polygon(x=c(xmin,xmax,xmax,xmin),y=c(ymin,ymin,ymax,ymax))
		} else {
			stop("triangular ends not yet implemented")
		}
		
	} else {
		
		stop("horizontal colbar not yet implemented!")
		
	}
	
	dev.off()
	
}
