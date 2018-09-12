sl.plot.init <-
function (projection="lonlat",lonlat.lonrange=c(-180,180),lonlat.latrange=c(-85,85),polar.lonlatrot=c(0,90,0),polar.latbound=0,regpoly.lonlatrot=c(0,90,0),regpoly.N=3,regpoly.lat0=60,regpoly.rotfrac=0,col.background=NULL,precision=1,main="",xshift=0,yshift=0,device="pdf",do.init=TRUE,do.init.device=do.init,file.name=paste0("~/sl.plot.",device),width=12) {
	
	pir = list(projection=projection)
	
	if (projection == "lonlat") {
		xlab = ""
		ylab = ""
		if (lonlat.lonrange[2] <= lonlat.lonrange[1]) {stop("lonlat.lonrange must be given in increasing order")}
		if (lonlat.lonrange[2] > 360 || lonlat.lonrange[1] <= -360) {stop("lonlat.lonrange must be within (-360,360]")}
		if (lonlat.lonrange[2] - lonlat.lonrange[1] > 360) {stop("lonlat.lonrange spans more than 360 degree")}
		if (max(abs(lonlat.latrange)) > 89) {stop("lonlat.latrange must be within [-89,89]")}
		if (lonlat.latrange[2] <= lonlat.latrange[1]) {stop("lonlat.latrange must be given in increasing order")}
		lat.span = lonlat.latrange
		xlim = extendrange(lonlat.lonrange,f=0.1)
		ylim = extendrange(lonlat.latrange,f=0.1)
		pir$lonlat.lonrange = lonlat.lonrange
		pir$lonlat.latrange = lonlat.latrange
	} else if (projection == "polar") {
		xlab = ""
		ylab = ""
		if (polar.latbound < 0 || polar.latbound >= 90) {stop("polar.latbound must be in [0,90)")}
		xlim = extendrange(c(sin(pi*(polar.latbound-90)/180),sin(pi*(90-polar.latbound)/180)),f=0.1)
		ylim = xlim
		abg = sl.lonlatrot2abg(polar.lonlatrot)
		pir$polar.lonlatrot = polar.lonlatrot
		pir$polar.latbound = polar.latbound
		pir$alpha = abg[1]
		pir$beta = abg[2]
		pir$gamma = abg[3]
	} else if (projection == "regpoly") {
		xlab = ""
		ylab = ""
		if (regpoly.lat0 <= 0 || regpoly.lat0 >= 90) {stop("regpoly.lat0 must be in (0,90)")}
		regpoly.N = round(regpoly.N)
		if (regpoly.N < 3) {stop("regpoly.N must be >= 3")}
		xlim = extendrange(c(sin(pi*(regpoly.lat0-90)/180),sin(pi*(90-regpoly.lat0)/180)),f=0.1)
		ylim = xlim
		abg = sl.lonlatrot2abg(regpoly.lonlatrot)
		pir$regpoly.lonlatrot = regpoly.lonlatrot
		pir$regpoly.N = regpoly.N
		pir$regpoly.lat0 = regpoly.lat0
		pir$alpha = abg[1]
		pir$beta = abg[2]
		pir$gamma = abg[3]
		pir$regpoly.z0 = sin(pi*regpoly.lat0/180)
		pir$regpoly.rotfrac = regpoly.rotfrac
		pir$regpoly.cornerlons0 = (seq(0,regpoly.N-1)+regpoly.rotfrac)*360/regpoly.N
		pir$regpoly.lat0i = sl.p2p(pir$regpoly.cornerlons0[1],regpoly.lat0,pir$regpoly.cornerlons0[2],regpoly.lat0,0.5)$lat
		rot.inv = sl.rot(pir$regpoly.cornerlons0,regpoly.lat0,pir$alpha,pir$beta,pir$gamma,invert=TRUE)
		pir$regpoly.cornerlons = rot.inv$lon
		pir$regpoly.cornerlats = rot.inv$lat
	} else {
		stop("projections other than 'lonlat', 'polar', and 'regpoly' not yet implemented")
	}
	
	if (do.init.device) {
		height = width * (ylim[2]-ylim[1]) / (xlim[2]-xlim[1])
		dev.fun = match.fun(device,descend=FALSE)
		dev.fun(file.name, width, height)
	}
	if (do.init) {
	  par(mar=rep(0,4))
	  plot(x=NULL,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,main=main,xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
	}
	
	pir$xlim = xlim
	pir$ylim = ylim
	pir$xshift = xshift
	pir$yshift = yshift
	
	if (!is.null(col.background)) {
	  if (projection == "polar") {
	    xyrad = sin(pi*(90-polar.latbound)/180)
	    x = c(-cos(seq(0,2*pi,2*pi*precision/360))) * xyrad
	    y = c(sin(seq(0,2*pi,2*pi*precision/360))) * xyrad
	    polygon(x+xshift,y+yshift,col=col.background,border=NA)
	  } else if (projection == "lonlat") {
	    rect(lonlat.lonrange[1]+xshift,lonlat.latrange[1]+yshift,lonlat.lonrange[2]+xshift,lonlat.latrange[2]+yshift,col=col.background,border=NA)
	  } else if (projection == "regpoly") {
	    lon.ext = c(rot.inv$lon,rot.inv$lon[1])
	    lat.ext = c(rot.inv$lat,rot.inv$lat[1])
	    sl.plot.polygon(pir,lon.ext,lat.ext,col.fill=col.background,border=FALSE,ignore.visibility=TRUE)
	  }
	}
	
	return(pir)
	
}
