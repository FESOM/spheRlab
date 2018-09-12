sl.plot.platon.init <-
function(body.type="hexahedron",width=60,skip.faces=NULL,col.background=NULL,device="pdf",do.init=TRUE,do.init.device=do.init,file.name=paste0("~/sl.plot.platon.",device)) {
	
	if (body.type == "hexahedron") {
		print("initialising hexahedron (cube) plot")
		faces.N = 6
		regpoly.N = 4
		lat0 = sl.xyz2lonlat(c(1,1,1))[2]
		deltaxy = 2/sqrt(3)
		xlim = extendrange(deltaxy*c(-1.5,2.5),f=0.1)
		ylim = extendrange(deltaxy*c(-1.5,1.5),f=0.1)
		height = width * (ylim[2]-ylim[1]) / (xlim[2]-xlim[1])
		faces.list = vector("list",faces.N)
		pir.list = vector("list",faces.N)
		faces.list[[1]] = list(   lonlatrot=c(0,90,0),rotfrac=.5,        xshift=0,       yshift=0)
		faces.list[[2]] = list( lonlatrot=c(90,0,-90),rotfrac=.5,  xshift=deltaxy,       yshift=0)
		faces.list[[3]] = list(lonlatrot=c(0,-90,180),rotfrac=.5,xshift=2*deltaxy,       yshift=0)
		faces.list[[4]] = list( lonlatrot=c(-90,0,90),rotfrac=.5, xshift=-deltaxy,       yshift=0)
		faces.list[[5]] = list(    lonlatrot=c(0,0,0),rotfrac=.5,        xshift=0,yshift=-deltaxy)
		faces.list[[6]] = list(lonlatrot=c(180,0,180),rotfrac=.5,        xshift=0, yshift=deltaxy)
	} else if (body.type == "icosahedron") {
		print("initialising icosahedron plot")
		faces.N = 20
		regpoly.N = 3
		deltaang = atan(.5)*180/pi
		golden = (1+sqrt(5))/2
		v1 = c(0,1,golden)
		v1 = v1 / sqrt(sum(v1^2))
		v2 = c(0,-1,golden)
		v2 = v2 / sqrt(sum(v2^2))
		v3 = c(golden,0,1)
		v3 = v3 / sqrt(sum(v3^2))
		deltaxy = sl.cart.dist(v1,v2)
		lat0 = 90 - sl.xyz.angle(v1,v1+v2+v3)
		xlim = extendrange(deltaxy*c(-0.5,5),f=0.1)
		ylim = extendrange(deltaxy*sin(pi/3)*c(-5/3,4/3),f=0.1)
		height = width * (ylim[2]-ylim[1]) / (xlim[2]-xlim[1])
		faces.list = vector("list",faces.N)
		pir.list = vector("list",faces.N)
		lat1.xyz = c(sl.lonlat2xyz(c(0,deltaang))+sl.lonlat2xyz(c(36,-deltaang))+sl.lonlat2xyz(c(72,deltaang)))
		lat1 = sl.xyz2lonlat(lat1.xyz)[2]
		lonlatrot.lon.vec = c(seq(0,324,36),seq(0,288,72),seq(36,324,72))
		lonlatrot.lat.vec = c(rep(c(lat1,-lat1),5),rep(lat0,5),rep(-lat0,5))
		rotfrac.vec = c(rep(c(0.25,-0.25),5),rep(-0.25,5),rep(0.25,5))
		xshift.vec = deltaxy*c(seq(0,9)/2,seq(0,4),seq(0,4)+0.5)
		yshift.vec = deltaxy*sin(pi/3)*c(rep(c(0,-1/3),5),rep(2/3,5),rep(-1,5))
		for (n in 1:faces.N) {
			faces.list[[n]] = list(lonlatrot=c(lonlatrot.lon.vec[n],lonlatrot.lat.vec[n],0),rotfrac=rotfrac.vec[n],xshift=xshift.vec[n],yshift=yshift.vec[n])
		}
	} else {
		stop("unknown body")
	}
  
  if (do.init.device) {
    dev.fun = match.fun(device,descend=FALSE)
    dev.fun(file.name,width=width,height=height)
  }
  if (do.init) {
    par(mar=rep(0,4))
    plot(x=NULL,xlim=xlim,ylim=ylim,xlab="",ylab="",main="",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
  }
	
	for (n in 1:faces.N) {
	  col.bg = col.background
	  if (!is.null(skip.faces) && n %in% skip.faces) {col.bg = NULL}
		pir.list[[n]] = sl.plot.init(projection="regpoly",regpoly.lat0=lat0,main="",regpoly.lonlatrot=faces.list[[n]]$lonlatrot,regpoly.N=regpoly.N,regpoly.rotfrac=faces.list[[n]]$rotfrac,col.background=col.bg,xshift=faces.list[[n]]$xshift,yshift=faces.list[[n]]$yshift,do.init=FALSE)
	}
	
	if (!is.null(skip.faces)) {
		pir.list.old = pir.list
		pir.list = list()
		nx = 0
		for (n in 1:faces.N) {
			if (!(n %in% skip.faces)) {
				nx = nx + 1
				pir.list[[nx]] = pir.list.old[[n]]
			}
		}
	}
	
	pir.list$projection = "platon"
	pir.list$body.type = body.type
	pir.list$deltaxy = deltaxy
	
	return(pir.list)
	
}
