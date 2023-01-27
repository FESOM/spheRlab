sl.plot.platon.end <-
function(plot.platon.init.res,corners=TRUE,corners.length=0.05,adhes.surf=TRUE,outer.boundaries=FALSE,inner.boundaries=FALSE,col.adh="grey",lty.adh=1,lwd.adh=0.5,col.bnd="grey",lty.bnd=1,lwd.bnd=0.5,do.close.device=TRUE) {
	
	deltaxy = plot.platon.init.res$deltaxy
	
	if (plot.platon.init.res$body.type == "tetrahedron") {
	  if (adhes.surf) {
	    l.x = deltaxy *             c(  -1, -1.1,-0.7,-0.5,NA,  0,0.2, 0.6,0.5,NA,   1,   0.9,   0.1,   0)
	    l.y = deltaxy * sin(pi/3) * c(-2/3,-7/15, 1/3, 1/3,NA,4/3,4/3,8/15,1/3,NA,-2/3,-13/15,-13/15,-2/3)
	    lines(x=l.x,y=l.y,col=col.adh,lty=lty.adh,lwd=lwd.adh)
	  }
	  l.x.ib = deltaxy *             c(-1/2,1/2,   0,-1/2)
	  l.y.ib = deltaxy * sin(pi/3) * c( 1/3,1/3,-2/3, 1/3)
	  if (inner.boundaries) {
	    lines(x=l.x.ib,y=l.y.ib,col=col.bnd,lty=lty.bnd,lwd=lwd.bnd)
	  }
	  l.x.ob = deltaxy *             c(  -1,  0,   1,  -1)
	  l.y.ob = deltaxy * sin(pi/3) * c(-2/3,4/3,-2/3,-2/3)
	  if (outer.boundaries) {
	    lines(x=l.x.ob,y=l.y.ob,col=col.bnd,lty=lty.bnd,lwd=lwd.bnd)
	  }
	} else if (plot.platon.init.res$body.type == "hexahedron") {
		if (adhes.surf) {
		  if (plot.platon.init.res$extra.face) {
			  l.x = deltaxy * c(-0.5,-0.7,-1.3,-1.5,-1.7,-1.7,-1.5,-1.3,-0.7,-0.5, NA, 0.5, 0.7, 1.3, 1.5, 1.7, 2.3, 2.5, NA, 1.5, 1.3, 0.7, 0.5, NA, 0.5, 0.3,-0.3,-0.5)
			  l.y = deltaxy * c(-0.5,-0.7,-0.7,-0.5,-0.3, 0.3, 0.5, 0.7, 0.7, 0.5, NA, 0.5, 0.7, 0.7, 0.5, 0.7, 0.7, 0.5, NA,-0.5,-0.7,-0.7,-0.5, NA,-1.5,-1.7,-1.7,-1.5)
		  } else {
		    l.x = deltaxy * c(-0.5,-0.7,-1.3,-1.5,-1.7,-1.7,-1.5,-1.3,-0.7,-0.5, NA, 0.5, 0.7, 1.3, 1.5, 1.7, 2.3, 2.5, NA, 2.5, 2.3, 1.7, 1.5, 1.3, 0.7, 0.5)
		    l.y = deltaxy * c(-0.5,-0.7,-0.7,-0.5,-0.3, 0.3, 0.5, 0.7, 0.7, 0.5, NA, 0.5, 0.7, 0.7, 0.5, 0.7, 0.7, 0.5, NA,-0.5,-0.7,-0.7,-0.5,-0.7,-0.7,-0.5)
		  }
			lines(x=l.x,y=l.y,col=col.adh,lty=lty.adh,lwd=lwd.adh)
		}
		l.x.ib = deltaxy * c(-0.5,-0.5, 0.5, 0.5,-0.5,  NA, 1.5, 1.5)
		l.y.ib = deltaxy * c(-0.5, 0.5, 0.5,-0.5,-0.5,  NA,-0.5, 0.5)
		if (inner.boundaries) {
			lines(x=l.x.ib,y=l.y.ib,col=col.bnd,lty=lty.bnd,lwd=lwd.bnd)
		}
		l.x.ob = deltaxy * c(-1.5,-0.5,-0.5, 0.5, 0.5, 1.5, 2.5, 2.5, 1.5, 0.5, 0.5,-0.5,-0.5,-1.5,-1.5)
		l.y.ob = deltaxy * c( 0.5, 0.5, 1.5, 1.5, 0.5, 0.5, 0.5,-0.5,-0.5,-0.5,-1.5,-1.5,-0.5,-0.5, 0.5)
		if (outer.boundaries) {
			lines(x=l.x.ob,y=l.y.ob,col=col.bnd,lty=lty.bnd,lwd=lwd.bnd)
		}
		if (plot.platon.init.res$extra.face) {
		  lines(x=deltaxy*c(1.5,1.5,2.5,2.5),y=deltaxy*c(-0.5,-1.5,-1.5,-0.5),col=col.bnd,lty=lty.bnd,lwd=lwd.bnd)
		}
	} else if (plot.platon.init.res$body.type == "icosahedron") {
		if (adhes.surf) {
			patt.x = c(0,0.2,0.55,0.5,NA)
			patt.y = c(1,0.95,0.25,0,NA)
			l.x = deltaxy * c(patt.x,patt.x+1,patt.x+2,patt.x+3,patt.x+4,-patt.x+1/2,-patt.x+3/2,-patt.x+5/2,-patt.x+7/2,-patt.x+9/2,-patt.x)
			l.y = deltaxy * sin(pi/3) * c(rep(patt.y+1/3,5),rep(-patt.y-2/3,5),-patt.y+1/3)
			lines(x=l.x,y=l.y,col=col.adh,lty=lty.adh,lwd=lwd.adh)
		}
		l.x.ib = deltaxy * c(seq(0,5)-0.5,seq(8,1)/2,seq(0,5))
		l.y.ib = deltaxy * sin(pi/3) * c(rep(1/3,6),rep(c(-2/3,1/3),4),rep(-2/3,6))
		if (inner.boundaries) {
			lines(x=l.x.ib,y=l.y.ib,col=col.bnd,lty=lty.bnd,lwd=lwd.bnd)
		}
		l.x.ob = deltaxy * c(seq(-1,9)/2,seq(10,-1)/2)
		l.y.ob = deltaxy * sin(pi/3) * c(rep(c(1/3,4/3),5),1/3,rep(c(-2/3,-5/3),5),-2/3,1/3)
		if (outer.boundaries) {
			lines(x=l.x.ob,y=l.y.ob,col=col.bnd,lty=lty.bnd,lwd=lwd.bnd)
		}
	} else if (plot.platon.init.res$body.type == "truncatedicosahedron") {
	  if (adhes.surf) {
	    patt.x = c(0,0.2,0.55,0.5,NA)
	    patt.y = c(1,0.95,0.25,0,NA)
	    l.x = deltaxy * c(patt.x,patt.x+1,patt.x+2,patt.x+3,patt.x+4,-patt.x+1/2,-patt.x+3/2,-patt.x+5/2,-patt.x+7/2,-patt.x+9/2,-patt.x)
	    l.y = deltaxy * sin(pi/3) * c(rep(patt.y+1/3,5),rep(-patt.y-2/3,5),-patt.y+1/3)
	    lines(x=l.x,y=l.y,col=col.adh,lty=lty.adh,lwd=lwd.adh)
	  }
	  #l.x.ib = deltaxy * c(seq(0,5)-0.5,seq(8,1)/2,seq(0,5))
	  #l.y.ib = deltaxy * sin(pi/3) * c(rep(1/3,6),rep(c(-2/3,1/3),4),rep(-2/3,6))
	  #if (inner.boundaries) {
	 #   lines(x=l.x.ib,y=l.y.ib,col=col.bnd,lty=lty.bnd,lwd=lwd.bnd)
	  #}
	  #l.x.ob = deltaxy * c(seq(-1,9)/2,seq(10,-1)/2)
	  #l.y.ob = deltaxy * sin(pi/3) * c(rep(c(1/3,4/3),5),1/3,rep(c(-2/3,-5/3),5),-2/3,1/3)
	  #if (outer.boundaries) {
	 #   lines(x=l.x.ob,y=l.y.ob,col=col.bnd,lty=lty.bnd,lwd=lwd.bnd)
	  #}
	} else {
		print("boundary mesh not defined for this body, only closing device")
		corners = FALSE
	}
	
	if (corners) {
		l.x.ibob = c(l.x.ib,NA,l.x.ob)
		l.y.ibob = c(l.y.ib,NA,l.y.ob)
		l.x = NULL
		l.y = NULL
		for (n in 1:(length(l.x.ibob)-1)) {
			l.x.n = l.x.ibob[n]
			l.x.diff = l.x.ibob[n+1] - l.x.n
			l.x = c(l.x,l.x.n,l.x.n+corners.length*l.x.diff,NA,l.x.n+(1-corners.length)*l.x.diff)
			l.y.n = l.y.ibob[n]
			l.y.diff = l.y.ibob[n+1] - l.y.n
			l.y = c(l.y,l.y.n,l.y.n+corners.length*l.y.diff,NA,l.y.n+(1-corners.length)*l.y.diff)
		}
		l.x = c(l.x,l.x.ibob[n+1])
		l.y = c(l.y,l.y.ibob[n+1])
		lines(x=l.x,y=l.y,col=col.bnd,lty=lty.bnd,lwd=lwd.bnd)
	}
	
	if (do.close.device) {dev.off()}
	
}
