sl.plot.colbar <-
function (colbar,categorical=FALSE,breaks=NULL,vertical=TRUE,labels.at=NULL,labels.num=min(length(breaks),5),labels=NULL,
          labels.side="bottom",labels.cex=1,ticks.length=1,ticks.mirrored=FALSE,ratio=.1,triag.ends=FALSE,device="pdf",
          do.init.device=TRUE,do.close.device=do.init.device,file.name=paste0("~/sl.plot.colbar.",device),width=6) {

  if (!is.null(names(colbar)) && "colbar" %in% names(colbar)) {
    if ("categorical" %in% names(colbar)) {categorical = colbar$categorical}
    if ("breaks" %in% names(colbar)) {breaks = colbar$breaks}
    if ("labels" %in% names(colbar)) {labels = colbar$labels}
    colbar = colbar$colbar
  }
  
  if (!categorical) {
    if (is.null(breaks)) {stop("'breaks' must be provided for non-categorical colourbar")}
    if (length(breaks) != length(colbar) - 1) {
      stop("'breaks' must be shorter by exactly one element compared to 'colbar'")
    }
  } else {
    if (!is.null(breaks)) {warning("'breaks' are ignored for categorical colourbar")}
    if (length(colbar) > 1) {breaks = seq(1.5, 1.5+length(colbar)-2,by=1)}
  }

  if (is.null(labels.at)) {
    if (categorical) {
      labels.at = rep(TRUE,length(colbar))
    } else {
      labels.at = rep(FALSE,length(breaks))
      labels.at[round(seq(1,length(breaks),(length(breaks)-1)/(labels.num-1)))] = TRUE
    }
  }

  if (is.null(labels)) {
    if (categorical) {
      labels = as.character(1:length(colbar))
    } else {
      labels = as.character(signif(breaks,3))
    }
  }

  if (do.init.device) {
    dev.fun = match.fun(device,descend=FALSE)
    dev.fun(file.name,width=width,height=width)
  }

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
		if (categorical && labels.at[1]) {
		  text(x=1.25*xmax,y=(ymi+yma)/2,labels=labels[1],pos=4,cex=labels.cex)
		}
		for (i in 2:length(colbar)) {
			ymi = ymi + ystp
			yma = ymi + ystp
			polygon(x=c(xmin,xmax,xmax,xmin),y=c(ymi,ymi,yma,yma),col=colbar[[i]],border=colbar[[i]],lwd=.01)
			if (categorical) {
			  if (labels.at[i]) {
			    text(x=1.25*xmax,y=(ymi+yma)/2,labels=labels[i],pos=4,cex=labels.cex)
			  }
			}
			else if (labels.at[i-1]) {
				text(x=1.25*xmax,y=ymi,labels=labels[i-1],pos=4,cex=labels.cex)
			}
		}

		if (!triag.ends) {
			polygon(x=c(xmin,xmax,xmax,xmin),y=c(ymin,ymin,ymax,ymax))
		} else {
			stop("triangular ends not yet implemented")
		}

	} else {

	  xmin = -0.3
	  xmax = 0.3
	  xstp = (xmax - xmin) / (length(breaks) + 1)
	  ymax = (xmax - xmin) * ratio / 2
	  ymin = -ymax

	  xmi = xmin
	  xma = xmi + xstp
	  polygon(x=c(xmi,xma,xma,xmi),y=c(ymin,ymin,ymax,ymax),col=colbar[[1]],border=colbar[[1]],lwd=.01)
	  if (categorical && labels.at[1]) {
	    text(x=(xmi+xma)/2,y=1.25*ymin,labels=labels[1],pos=1,cex=labels.cex)
	  }
	  for (i in 2:length(colbar)) {
	    xmi = xmi + xstp
	    xma = xmi + xstp
	    polygon(x=c(xmi,xma,xma,xmi),y=c(ymin,ymin,ymax,ymax),col=colbar[[i]],border=colbar[[i]],lwd=.01)
	    if (categorical) {
	      if (labels.at[i]) {
	        text(x=(xmi+xma)/2,y=1.25*ymin,labels=labels[i],pos=1,cex=labels.cex)
	      }
	    } else if (labels.at[i-1]) {
	      text(x=xmi,y=1.25*ymin,labels=labels[i-1],pos=1,cex=labels.cex)
	    }
	  }

	  if (!triag.ends) {
	    polygon(x=c(xmin,xmax,xmax,xmin),y=c(ymin,ymin,ymax,ymax))
	  } else {
	    stop("triangular ends not yet implemented")
	  }

	}

	if (do.close.device) {dev.off()}

}
