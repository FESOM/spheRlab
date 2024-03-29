sl.plot.colbar <-
function (colbar,categorical=FALSE,breaks=NULL,vertical=TRUE,labels.at=NULL,labels.num=min(length(breaks),9),labels=NULL,
          labels.side="bottom",labels.cex=1,labels.signif=3,ticks.length=1,ticks.mirrored=FALSE,ratio=.1,triag.ends=FALSE,device="pdf",
          do.init=TRUE,do.init.device=do.init,do.close.device=do.init.device,file.name=paste0("~/sl.plot.colbar.",device),width=6,
          xshift=0,yshift=0,len=0.6,units=NULL,units.xy=NULL,labels.col="black") {

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
      if (is.null(labels.num)) {
        labels.at = rep(TRUE,length(breaks))
      } else {
        labels.at = rep(FALSE,length(breaks))
        labels.at[round(seq(1,length(breaks),(length(breaks)-1)/(labels.num-1)))] = TRUE
      }
    }
  }

  if (is.null(labels)) {
    if (categorical) {
      labels = as.character(1:length(colbar))
    } else {
      labels = as.character(signif(breaks,labels.signif))
    }
  }

  if (do.init.device) {
    if (device %in% c("bmp","jpeg","png","tiff")) {width = width * 100}
    dev.fun = match.fun(device,descend=FALSE)
    dev.fun(file.name,width=width,height=width)
  }

  if (do.init) {
	  par(mar=rep(0,4))
	  plot(NA,xlim=c(-0.5,0.5),ylim=c(-0.5,0.5),bty="n",xaxt="n",yaxt="n")
  }

	if (vertical) {

		ymin = -len/2 + yshift
		ymax = len/2 + yshift
		ystp = (ymax - ymin) / (length(breaks) + 1)
		xmax = (ymax - ymin) * ratio / 2
		xmin = -xmax + xshift
		xmax = xmax + xshift
		xstp = xmax - xmin

		ymi = ymin
		yma = ymi + ystp
		if (!triag.ends) {
		  polygon(x=c(xmin,xmax,xmax,xmin),y=c(ymi,ymi,yma,yma),col=colbar[[1]],border=colbar[[1]],lwd=.01)
		} else {
		  polygon(x=c(xmin,xmin+xstp/2,xmax),y=c(yma,yma-xstp,yma),col=colbar[[1]],border=colbar[[1]],lwd=.01)
		}
		if (categorical && labels.at[1]) {
		  text(x=xmax+(xmax-xmin)/8,y=(ymi+yma)/2,labels=labels[1],pos=4,cex=labels.cex,col=labels.col)
		}
		for (i in 2:length(colbar)) {
			ymi = ymi + ystp
			yma = ymi + ystp
			if (i < length(colbar) || !triag.ends) {
			  polygon(x=c(xmin,xmax,xmax,xmin),y=c(ymi,ymi,yma,yma),col=colbar[[i]],border=colbar[[i]],lwd=.01)
			} else {
			  polygon(x=c(xmin,xmax,xmin+xstp/2),y=c(ymi,ymi,ymi+xstp),col=colbar[[i]],border=colbar[[i]],lwd=.01)
			}
			if (categorical) {
			  if (labels.at[i]) {
			    text(x=xmax+(xmax-xmin)/8,y=(ymi+yma)/2,labels=labels[i],pos=4,cex=labels.cex,col=labels.col)
			  }
			}
			else if (labels.at[i-1]) {
				text(x=xmax+(xmax-xmin)/8,y=ymi,labels=labels[i-1],pos=4,cex=labels.cex,col=labels.col)
			}
		}

		if (!triag.ends) {
			polygon(x=c(xmin,xmax,xmax,xmin),y=c(ymin,ymin,ymax,ymax))
		} else {
		  polygon(x=c(xmin,xmin+xstp/2,xmax,xmax,xmin+xstp/2,xmin),
		          y=c(ymin+ystp,ymin+ystp-xstp,ymin+ystp,ymax-ystp,ymax-ystp+xstp,ymax-ystp))
		}
		
		if (!is.null(units)) {
		  if (is.null(units.xy)) {units.xy = c(0,1.2)}
		  text(x=units.xy[1]*len/2+xshift,y=units.xy[2]*len/2+yshift,labels=units,cex=labels.cex,col=labels.col)
		}

	} else {

	  xmin = -len/2 + xshift
	  xmax = len/2 + xshift
	  xstp = (xmax - xmin) / (length(breaks) + 1)
	  ymax = (xmax - xmin) * ratio / 2
	  ymin = -ymax + yshift
	  ymax = ymax + yshift
	  ystp = ymax - ymin

	  xmi = xmin
	  xma = xmi + xstp
	  if (!triag.ends) {
	    polygon(x=c(xmi,xma,xma,xmi),y=c(ymin,ymin,ymax,ymax),col=colbar[[1]],border=colbar[[1]],lwd=.01)
	  } else {
	    polygon(x=c(xma-ystp,xma,xma),y=c(ymin+ystp/2,ymin,ymax),col=colbar[[1]],border=colbar[[1]],lwd=.01)
	  }
	  if (categorical && labels.at[1]) {
	    text(x=(xmi+xma)/2,y=ymin-(ymax-ymin)/8,labels=labels[1],pos=1,cex=labels.cex,col=labels.col)
	  }
	  for (i in 2:length(colbar)) {
	    xmi = xmi + xstp
	    xma = xmi + xstp
	    if (i < length(colbar) || !triag.ends) {
	      polygon(x=c(xmi,xma,xma,xmi),y=c(ymin,ymin,ymax,ymax),col=colbar[[i]],border=colbar[[i]],lwd=.01)
	    } else {
	      polygon(x=c(xmi,xmi+ystp,xmi),y=c(ymin,ymin+ystp/2,ymax),col=colbar[[i]],border=colbar[[i]],lwd=.01)
	    }
	    if (categorical) {
	      if (labels.at[i]) {
	        text(x=(xmi+xma)/2,y=ymin-(ymax-ymin)/8,labels=labels[i],pos=1,cex=labels.cex,col=labels.col)
	      }
	    } else if (labels.at[i-1]) {
	      text(x=xmi,y=ymin-(ymax-ymin)/8,labels=labels[i-1],pos=1,cex=labels.cex,col=labels.col)
	    }
	  }

	  if (!triag.ends) {
	    polygon(x=c(xmin,xmax,xmax,xmin),y=c(ymin,ymin,ymax,ymax))
	  } else {
	    polygon(x=c(xmin+xstp-ystp,xmin+xstp,xmax-xstp,xmax-xstp+ystp,xmax-xstp,xmin+xstp),
	            y=c(ymin+ystp/2,ymin,ymin,ymin+ystp/2,ymax,ymax))
	  }
	  
	  if (!is.null(units)) {
	    if (is.null(units.xy)) {units.xy = c(1.2,0)}
	    text(x=units.xy[1]*len/2+xshift,y=units.xy[2]*len/2+yshift,labels=units,cex=labels.cex,col=labels.col)
	  }

	}

	if (do.close.device) {dev.off()}

}
