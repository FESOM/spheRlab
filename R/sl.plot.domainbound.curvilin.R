sl.plot.domainbound.curvilin <-
function (plot.init.res,lon.i,lat.i,col="black",lwd=1,lty=1,ignore.visibility=FALSE) {
	
	M = dim(lon.i)[1]
	N = dim(lon.i)[2]
	for (m in 1:(M-1)) {
		sl.plot.lines(plot.init.res,lon.i[c(m,m+1),1],lat.i[c(m,m+1),1],col=col,lwd=lwd,lty=lty,ignore.visibility=FALSE)
		sl.plot.lines(plot.init.res,lon.i[c(m,m+1),N],lat.i[c(m,m+1),N],col=col,lwd=lwd,lty=lty,ignore.visibility=FALSE)
	}
	for (n in 1:(N-1)) {
		sl.plot.lines(plot.init.res,lon.i[1,c(n,n+1)],lat.i[1,c(n,n+1)],col=col,lwd=lwd,lty=lty,ignore.visibility=FALSE)
		sl.plot.lines(plot.init.res,lon.i[M,c(n,n+1)],lat.i[M,c(n,n+1)],col=col,lwd=lwd,lty=lty,ignore.visibility=FALSE)
	}
	
}
