sl.plot.platon.init <-
function(body.type="hexahedron",width=60,skip.faces=NULL,col.background=NULL,device="pdf",do.init=TRUE,do.init.device=do.init,file.name=paste0("~/sl.plot.platon.",device),transform.function=NULL,extra.face=FALSE) {
	
  if (body.type == "tetrahedron") {
    print("initialising tetrahedron plot")
    faces.N = 4
    regpoly.N = 3
    lat0 = -sl.xyz2lonlat(c(sqrt(8/9), 0, -1/3))[2]
    deltaxy = sl.cart.dist(c(sqrt(8/9), 0, -1/3),c(0,0,1))
    xlim = extendrange(deltaxy*c(-1,1),f=0.1)
    ylim = extendrange(deltaxy*sin(pi/3)*c(-2/3-1/10,4/3),f=0.1)
    height = width * (ylim[2]-ylim[1]) / (xlim[2]-xlim[1])
    faces.list = vector("list",faces.N)
    pir.list = vector("list",faces.N)
    lonlatrot.lon.vec = c(0,40,160,-80)
    lonlatrot.lat.vec = c(-90,lat0,lat0,lat0)
    lonlatrot.rot.vec = c(-160,-120,0,120)
    rotfrac.vec = c(0.25,rep(-0.25,3))
    xshift.vec = deltaxy/2 * c(0,-1,0,1)
    yshift.vec = deltaxy*sin(pi/3) * c(0,-1/3,2/3,-1/3)
    for (n in 1:faces.N) {
      faces.list[[n]] = list(lonlatrot = c(lonlatrot.lon.vec[n], lonlatrot.lat.vec[n], lonlatrot.rot.vec[n]),
                             rotfrac = rotfrac.vec[n],
                             xshift = xshift.vec[n],
                             yshift = yshift.vec[n],
                             lat0 = lat0,
                             regpoly.N = regpoly.N)
    }
  } else if (body.type == "hexahedron") {
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
		lonlatrot.lon.vec = c(0,90,0,-90,0,180)
		lonlatrot.lat.vec = c(90,0,-90,0,0,0)
		lonlatrot.rot.vec = c(0,-90,180,90,0,180)
		rotfrac.vec = rep(0.5,6)
		xshift.vec = c(0,deltaxy,2*deltaxy,-deltaxy,0,0)
		yshift.vec = c(0,0,0,0,-deltaxy,deltaxy)
		for (n in 1:faces.N) {
		  faces.list[[n]] = list(lonlatrot = c(lonlatrot.lon.vec[n], lonlatrot.lat.vec[n], lonlatrot.rot.vec[n]),
		                         rotfrac = rotfrac.vec[n],
		                         xshift = xshift.vec[n],
		                         yshift = yshift.vec[n],
		                         lat0 = lat0,
		                         regpoly.N = regpoly.N)
		}
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
			faces.list[[n]] = list(lonlatrot = c(lonlatrot.lon.vec[n], lonlatrot.lat.vec[n], 0),
			                       rotfrac = rotfrac.vec[n],
			                       xshift = xshift.vec[n],
			                       yshift = yshift.vec[n],
			                       lat0 = lat0,
			                       regpoly.N = regpoly.N)
		}
	} else if (body.type == "truncatedicosahedron") {
	  print("initialising truncated icosahedron plot")
	  faces.N = 32
	  golden = (1+sqrt(5))/2
	  v1 = c(0,1,golden)
	  v1 = v1 / sqrt(sum(v1^2))
	  v2 = c(0,-1,golden)
	  v2 = v2 / sqrt(sum(v2^2))
	  v3 = c(golden,0,1)
	  v3 = v3 / sqrt(sum(v3^2))
	  v4 = (2*v1 + v2) / 3
	  #v4 = v4 / sqrt(sum(v4^2))
	  v5 = (2*v1 + v3) / 3
	  #v5 = v5 / sqrt(sum(v5^2))
	  v6 = (v1 + v2 + v3) / 3
	  deltax = 3 * sl.cart.dist(v4,v5) / sqrt(sum(v5^2))
	  deltaxy = deltax
	  deltay = 2/3 * deltax * cos(pi/6)
	  v45 = (v4 + v5) / 2
	  #deltay.2 = sl.cart.dist(v1, v45/sqrt(sum(v45^2))/cos(sl.xyz.angle(v1,v45)*2*pi/360))
	  deltay.2 = deltax * tan(3*pi/10) / 6
	  lat0.6 = 90 - sl.xyz.angle(v4,v1+v2+v3)
	  lat0.5 = 90 - sl.xyz.angle(v4,v1)
	  xlim = extendrange(deltax*c(-0.2,4.7),f=0.1)
	  ylim = extendrange(deltay*c(-1.2,3.7),f=0.1)
	  height = width * (ylim[2]-ylim[1]) / (xlim[2]-xlim[1])
	  faces.list = vector("list",faces.N)
	  pir.list = vector("list",faces.N)
	  regpoly.N.vec = c(6,6,5,5,6,6)
	  regpoly.N.vec = c(regpoly.N.vec, 5, 5, rep(regpoly.N.vec,4))
	  lat0.vec = c(lat0.6,lat0.6,lat0.5,lat0.5,lat0.6,lat0.6)
	  lat0.vec = c(lat0.vec, lat0.5, lat0.5, rep(lat0.vec,4))
	  ang56 = sl.xyz.angle(v1,v1+v2+v3)
	  ang66 = 2 * sl.xyz.angle(v1+v2,v1+v2+v3)
	  lat3 = 90 - ang56
	  lat1 = lat3 - ang66
	  lat2 = -lat1 + ang56
	  lonlatrot.lon.vec = c(rep(0,3),rep(1,4),rep(2,4),rep(seq(3,9),each=3)) * 36
	  lonlatrot.lat.vec = c(-lat3,-lat1,lat2,-lat2,lat1,lat3)
	  lonlatrot.lat.vec = c(lonlatrot.lat.vec, 90, -90, rep(lonlatrot.lat.vec, 4))
	  rotfrac.vec = c(0,0,0.25,-0.25,0,0)
	  rotfrac.vec = c(rotfrac.vec, 0.25, -0.25, rep(rotfrac.vec, 4))
	  xshift.vec = deltax/2 * lonlatrot.lon.vec/36
	  yshift.vec = c(0, deltay, 3/2*deltay+deltay.2, deltay-deltay.2, 3/2*deltay, 5/2*deltay)
	  yshift.vec = c(yshift.vec, 3*deltay+deltay.2, -(deltay/2+deltay.2), rep(yshift.vec, 4))
	  for (n in 1:faces.N) {
	    faces.list[[n]] = list(lonlatrot = c(lonlatrot.lon.vec[n], lonlatrot.lat.vec[n], 0),
	                           rotfrac = rotfrac.vec[n],
	                           xshift = xshift.vec[n],
	                           yshift = yshift.vec[n],
	                           lat0 = lat0.vec[n],
	                           regpoly.N = regpoly.N.vec[n])
	  }
	} else {
		stop("unknown body")
	}
  
  if (do.init.device) {
    if (device %in% c("bmp","jpeg","png","tiff")) {width = width * 100}
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
		pir.list[[n]] = sl.plot.init(projection = "regpoly",
		                             regpoly.lat0 = faces.list[[n]]$lat0,
		                             main = "",
		                             regpoly.lonlatrot = faces.list[[n]]$lonlatrot,
		                             regpoly.N = faces.list[[n]]$regpoly.N,
		                             regpoly.rotfrac = faces.list[[n]]$rotfrac,
		                             col.background = col.bg,
		                             xshift = faces.list[[n]]$xshift,
		                             yshift = faces.list[[n]]$yshift,
		                             do.init = FALSE)
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
	pir.list$transform.function = transform.function
	pir.list$extra.face = extra.face
	
	return(pir.list)
	
}
