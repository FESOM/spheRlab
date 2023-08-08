sl.plot.init <-
function (projection="lonlat",lonrange=c(-180,180),latrange=c(-89,89),lonlatrot=NULL,polar.latbound=0,regpoly.N=3,regpoly.lat0=60,regpoly.rotfrac=0,col.background=NULL,precision=1,main="",xshift=0,yshift=0,device="pdf",do.init=TRUE,file.name=NULL,width=12,transform.function=NULL,mar=rep(0,4),lonlat.lonrange=c(-180,180),lonlat.latrange=c(-85,85),lonlat.lonlatrot=NULL,polar.lonlatrot=c(0,90,0),regpoly.lonlatrot=c(0,90,0),do.init.device=NULL) {
	
  # handling of deprecated arguments for backward compatibility
  if (missing(lonlatrot)) {
    if (projection=="lonlat") {lonlatrot = lonlat.lonlatrot}
    else if (projection=="polar") {lonlatrot = polar.lonlatrot}
    else if (projection=="regpoly") {lonlatrot = regpoly.lonlatrot}
  }
  if (projection=="lonlat" && missing(lonrange)) {lonrange = lonlat.lonrange}
  if (projection=="lonlat" && missing(latrange)) {latrange = lonlat.latrange}
  
  if (!is.null(do.init.device)) {
    warning("Argument 'do.init.device' is deprecated. Specifying or not specifying 'file.name' is sufficient to determine if a (non-standard) device is initiated.")
    if (is.null(file.name)) {
      file.name = paste0("~/sl.plot.",device)
      warning(paste0("'file.name' is set to ",file.name))
    }
  } else {
    do.init.device = !is.null(file.name)
  }
  
	pir = list(projection=projection)
	
	if (projection %in% c("lonlat","mollweide")) {
		xlab = ""
		ylab = ""
		if (lonrange[2] <= lonrange[1]) {stop("lonrange must be given in increasing order")}
		if (lonrange[2] > 360 || lonrange[1] <= -360) {stop("lonrange must be within (-360,360]")}
		if (lonrange[2] - lonrange[1] > 360) {stop("lonrange spans more than 360 degree")}
		if (max(abs(latrange)) > 89) {stop("latrange must be within [-89,89]")}
		if (latrange[2] <= latrange[1]) {stop("latrange must be given in increasing order")}
		lat.span = latrange
		if (projection == "lonlat") {
		  xlim = extendrange(lonrange,f=0.1)
		  ylim = extendrange(latrange,f=0.1)
		}
		else if (projection == "mollweide") {
		  lonrange.mo = rep(lonrange,2); latrange.mo = rep(latrange, each=2)
		  if (latrange[1] < 0 && latrange[2] > 0) {
		    lonrange.mo = c(lonrange.mo, lonrange); latrange.mo = c(latrange.mo, 0, 0)
		  }
		  mollweide.corners = sl.proj.mollweide(lon = lonrange.mo, lat = latrange.mo)
		  xlim = extendrange(mollweide.corners$x,f=0.1)
		  ylim = extendrange(mollweide.corners$y,f=0.1)
		}
		pir$lonrange = lonrange
		pir$latrange = latrange
		pir$lonlatrot = lonlatrot
		if (!is.null(lonlatrot)) {
		  abg = sl.lonlatrot2abg(lonlatrot)
		  pir$alpha = abg[1]
		  pir$beta = abg[2]
		  pir$gamma = abg[3]
		} else {
		  pir$alpha = NULL
		  pir$beta = NULL
		  pir$gamma = NULL
		}
	} else if (projection == "polar") {
		xlab = ""
		ylab = ""
		if (polar.latbound < 0 || polar.latbound >= 90) {stop("polar.latbound must be in [0,90)")}
		xlim = extendrange(c(sin(pi*(polar.latbound-90)/180),sin(pi*(90-polar.latbound)/180)),f=0.1)
		ylim = xlim
		abg = sl.lonlatrot2abg(lonlatrot)
		pir$lonlatrot = lonlatrot
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
		abg = sl.lonlatrot2abg(lonlatrot)
		pir$lonlatrot = lonlatrot
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
	  if (device %in% c("bmp","jpeg","png","tiff")) {width = width * 100}
		height = width * (ylim[2]-ylim[1]) / (xlim[2]-xlim[1])
		dev.fun = match.fun(device,descend=FALSE)
		dev.fun(file.name, width*(1+mar[2]+mar[4]), height*(1+mar[1]+mar[3]))
	}
	if (do.init) {
	  par(mar=rep(0,4))
	  plot.xlim = xlim
	  plot.ylim = ylim
	  plot.ylim[1] = plot.ylim[1] - mar[1] * diff(ylim)
	  plot.xlim[1] = plot.xlim[1] - mar[2] * diff(xlim)
	  plot.ylim[2] = plot.ylim[2] + mar[3] * diff(ylim)
	  plot.xlim[2] = plot.xlim[2] + mar[4] * diff(xlim)
	  plot(x=NULL,xlim=plot.xlim,ylim=plot.ylim,xlab=xlab,ylab=ylab,main=main,xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
	}
	
	pir$xlim = xlim
	pir$ylim = ylim
	pir$xshift = xshift
	pir$yshift = yshift
	pir$transform.function = transform.function
	
	if (!is.null(col.background)) {
	  if (projection == "polar") {
	    xyrad = sin(pi*(90-polar.latbound)/180)
	    x = c(-cos(seq(0,2*pi,2*pi*precision/360))) * xyrad
	    y = c(sin(seq(0,2*pi,2*pi*precision/360))) * xyrad
	    polygon(x+xshift,y+yshift,col=col.background,border=NA)
	  } else if (projection == "lonlat") {
	    rect(lonlat.lonrange[1]+xshift,lonlat.latrange[1]+yshift,lonlat.lonrange[2]+xshift,lonlat.latrange[2]+yshift,col=col.background,border=NA)
	  } else if (projection == "mollweide") {
	    poly.lons = seq(lonrange[1],lonrange[2],precision)
	    if (tail(poly.lons,1) != lonrange[2]) {poly.lons = c(poly.lons,lonrange[2])}
	    Nlo = length(poly.lons)
	    poly.lats = seq(latrange[1],latrange[2],precision)
	    if (tail(poly.lats,1) != latrange[2]) {poly.lats = c(poly.lats,latrange[2])}
	    Nla = length(poly.lats)
	    xy = sl.proj.mollweide(lon = c(rep(lonrange[1],Nla), poly.lons[2:max(Nlo-1,2)], rep(lonrange[2],Nla), poly.lons[max(Nlo-1,2):2]),
	                   lat = c(poly.lats, rep(latrange[2],max(Nlo-2,1)), poly.lats, rep(latrange[1],max(Nlo-2,1))))
	    polygon(xy$x+xshift,xy$y+yshift,col=col.background,border=NA)
	  } else if (projection == "regpoly") {
	    lon.ext = c(rot.inv$lon,rot.inv$lon[1])
	    lat.ext = c(rot.inv$lat,rot.inv$lat[1])
	    sl.plot.polygon(pir,lon.ext,lat.ext,col.fill=col.background,border=FALSE,ignore.visibility=TRUE)
	  }
	}
	
	pir$do.init.device = do.init.device
	
	return(pir)
	
}
