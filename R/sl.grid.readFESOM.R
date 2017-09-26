sl.grid.readFESOM <-
function (griddir,rot=FALSE,rot.invert=FALSE,rot.abg,threeD=FALSE,remove.emptylev=TRUE,read.boundary=TRUE,reorder.ccw=TRUE,maxmaxneigh=12,findneighbours.maxiter=10,repeatlastpoint=TRUE,onlybaryc=FALSE,omitcoastnds=FALSE,calcpolyareas=TRUE,Rearth=6371000,verbose=TRUE) {
	
	if (verbose) {print("reading node (grid point) coordinates and coast information ...")}
	nod.scan = scan(paste0(griddir,"/nod2d.out"))
	N = as.integer(nod.scan[1])
	lon.orig = nod.scan[seq(3,N*4+1,4)]
	lat.orig = nod.scan[seq(4,N*4+1,4)]
	coast = as.logical(nod.scan[seq(5,N*4+1,4)]%%2)
	rm(nod.scan)
	if (verbose) {print(paste0("... done. grid contains ",N," nodes of which ",sum(coast)," are coastal."))}
	
	if (rot) {
		if (verbose) {print("rotating grid ...")}
		rot.res = sl.rot(lon.orig,lat.orig,rot.abg[1],rot.abg[2],rot.abg[3],return.xyz=TRUE,invert=rot.invert)
		lon = rot.res$lon
		lat = rot.res$lat
		x = rot.res$x
		y = rot.res$y
		z = rot.res$z
		rm(rot.res)
		if (verbose) {print("... done.")}
	} else {
		lon = lon.orig
		lat = lat.orig
		x = cos(lat*pi/180) * cos(lon*pi/180)
		y = cos(lat*pi/180) * sin(lon*pi/180)
		z = sin(lat*pi/180)
	}
	rm(lon.orig,lat.orig)
	
	lon[lon>180] = lon[lon>180] - 360
	lon[lon<=(-180)] = lon[lon<=(-180)] + 360
	
	if (verbose) {print("reading neighbourhood (triangular elements) information ...")}
	elem.scan = scan(paste(griddir,"/elem2d.out",sep=""))
	Ne = as.integer(elem.scan[1])
	elem = matrix(elem.scan[2:(Ne*3+1)],ncol=3,byrow=TRUE)
	rm(elem.scan)
	if (verbose) {print(paste0("... done. grid contains ",Ne," triangular elements."))}
	
	if (reorder.ccw) {
		if (verbose) {print(paste0("reordering clockwise triangular elements counterclockwise ..."))}
		ord.c = 0
		for (ie in 1:Ne) {
			a = c(lon[elem[ie,1]],lat[elem[ie,1]])
			b = c(lon[elem[ie,2]],lat[elem[ie,2]])
			c = c(lon[elem[ie,3]],lat[elem[ie,3]])
			if (sl.checkposition(a,b,c)==(-1)) {
				elem[ie,] = elem[ie,3:1]
				ord.c = ord.c + 1
			}
		}
		if (verbose) {print(paste0("... done. ",ord.c," of ",Ne," elements reordered."))}
	}
	
	N3D = NULL
	Nlev = NULL
	depth = NULL
	depth.lev = NULL
	boundary = NULL
	if (threeD) {
	  if (verbose) {print("reading 3D information ...")}
	  Nlev = scan(paste(griddir,"/aux3d.out",sep=""),n=1,what=integer())
	  aux3d.mat = matrix(scan(paste(griddir,"/aux3d.out",sep=""),na.strings="-999",skip=1,n=Nlev*N,what=integer()),ncol=Nlev,byrow=TRUE)
	  depth.lev = rep(Nlev,N)
    for (lev in Nlev:1) {
      isna.lev = is.na(aux3d.mat[,lev])
      if (remove.emptylev && sum(isna.lev) == N) {
        if (verbose) {print(paste("removing empty level",lev,"from data"))}
        Nlev = Nlev - 1
        aux3d.mat = aux3d.mat[,1:Nlev]
      }
      depth.lev[isna.lev] = depth.lev[isna.lev] - 1
	  }
	  N3D = sum(!is.na(aux3d.mat))
    if (verbose) {print("retrieving depth information from nod3d.out ...")}
	  nod3d.scan = scan(paste(griddir,"/nod3d.out",sep=""))
    depth = unique(nod3d.scan[seq(5,N3D*5+1,5)]) * -1
	  if (length(depth) != Nlev) { stop("data in aux3d.out is inconsistent with the number of depth levels") }
    if (read.boundary) {
      if (verbose) {print("retrieving 'coast/bottom' information from nod3d.out ...")}
      boundary = as.integer(nod3d.scan[seq(6,N3D*5+1,5)])
    }
    rm(nod3d.scan)
	}
	
	if (verbose) {print("searching all neighbours of each node based on the triangular elements ...")}
	findneighbours.res = sl.findneighbours(elem=elem,maxmaxneigh=12,reverse=FALSE,max.iter=findneighbours.maxiter)
	neighnodes = findneighbours.res$neighbour.nodes
	neighelems = findneighbours.res$neighbour.elems
	Nneighs = findneighbours.res$N.neighbour.nodes
	if (findneighbours.res$all.elements.arranged) {
		badnodes = NULL
	} else {
		badnodes = (1:N)[!findneighbours.res$elems.completed]
	}
	rm(findneighbours.res)
	if (verbose) {print(paste0("... done. number of neighbours ranges from ",min(Nneighs)," to ",max(Nneighs)," nodes and is ",signif(mean(Nneighs),digits=4)," on average."))}
	if (!is.null(badnodes)) {warning(paste0("if 'findneighbours.maxiter' was not set too low, the grid contains ",length(badnodes)," 'bad nodes'. consider increasing 'findneighbours.maxiter'. if the problem remains, the grid indeed contains bad nodes that should not exist in the first place. for such nodes only one part of the corresponding ocean patches will be returned by this function (which introduces a slight grid inaccuracy)."))}
	
	if (verbose) {print("computing barycenters (centroids) for all triangular elements ...")}
	baryc.lon = numeric(Ne)
	baryc.lat = numeric(Ne)
	for (ie in 1:Ne) {
		elem.ie = elem[ie,]
		baryc.res = sl.barycenter(x=x[elem.ie],y=y[elem.ie],z=z[elem.ie])
		baryc.lon[ie] = baryc.res$lon
		baryc.lat[ie] = baryc.res$lat
	}
	baryc.lon[baryc.lon>180] = baryc.lon[baryc.lon>180] - 360
	baryc.lon[baryc.lon<=(-180)] = baryc.lon[baryc.lon<=(-180)] + 360
	if (verbose) {print("... done.")}
	
	if (verbose) {print("generate 'stamp polygons' around each node ...")}
	maxneighs = ncol(neighnodes)
	maxNstamp = 2 * maxneighs
	stampmat.lon = matrix(NA,nrow=N,ncol=maxNstamp)
	stampmat.lat = matrix(NA,nrow=N,ncol=maxNstamp)
	Nstamp = rep(NA,N)
	for (i in 1:N) {
		Nstamp.i = 0
		for (j in 1:maxneighs) {
			nn = neighnodes[i,j]
			if (is.na(nn)) {
				break
			}
			if ( !onlybaryc || (coast[i] && ( j == 1 || j == Nneighs[i] ) ) ) {
				# compute median of central node and neighbour node
				medi = sl.barycenter(x=c(x[i],x[nn]),y=c(y[i],y[nn]),z=c(z[i],z[nn]))
				Nstamp.i = Nstamp.i + 1
				stampmat.lon[i,Nstamp.i] = medi$lon
				stampmat.lat[i,Nstamp.i] = medi$lat
			}
			ne = neighelems[i,j]
			if (is.na(ne)) {
				break
			}
			Nstamp.i = Nstamp.i + 1
			stampmat.lon[i,Nstamp.i] = baryc.lon[ne]
			stampmat.lat[i,Nstamp.i] = baryc.lat[ne]
		}
		if (coast[i] && !omitcoastnds) {
			Nstamp.i = Nstamp.i + 1
			stampmat.lon[i,Nstamp.i] = lon[i]
			stampmat.lat[i,Nstamp.i] = lat[i]
		}
		Nstamp[i] = Nstamp.i
	}
	if (maxNstamp > max(Nstamp)) {
		maxNstamp = max(Nstamp)
		stampmat.lon = stampmat.lon[,1:maxNstamp]
		stampmat.lat = stampmat.lat[,1:maxNstamp]
	}
	for (i in 1:N) {
		if (Nstamp[i] < maxNstamp) {
			if (repeatlastpoint) {
				stampmat.lon[i,(Nstamp[i]+1):maxNstamp] = stampmat.lon[i,Nstamp[i]]
				stampmat.lat[i,(Nstamp[i]+1):maxNstamp] = stampmat.lat[i,Nstamp[i]]
			} else {
				lon.endpoints = c(stampmat.lon[i,Nstamp[i]],stampmat.lon[i,1])
				lat.endpoints = c(stampmat.lat[i,Nstamp[i]],stampmat.lat[i,1])
				nfill = maxNstamp - Nstamp[i]
				fillequidist.res = sl.fillequidist(lon.endpoints,lat.endpoints,nfill)
				stampmat.lon[i,(Nstamp[i]+1):maxNstamp] = fillequidist.res$lon
				stampmat.lat[i,(Nstamp[i]+1):maxNstamp] = fillequidist.res$lat
			}
		}
	}
	stampmat.lon[stampmat.lon>180] = stampmat.lon[stampmat.lon>180] - 360
	stampmat.lon[stampmat.lon<=(-180)] = stampmat.lon[stampmat.lon<=(-180)] + 360
	if (verbose) {print(paste0("... done. number of 'stamp polygon' vertices per node ranges from ",min(Nstamp)," (before padding) to ",max(Nstamp)," and is ",signif(mean(Nstamp),digits=4)," on average (before padding)."))}
	
	cellareas = NULL
	elemareas = NULL
	if (calcpolyareas) {
		if (verbose) {print("computing element and 'stamp polygon' areas ...")}
		elemareas = rep(NA,Ne)
		cellareas = rep(NA,N)
		for (ie in 1:Ne) {
			elemareas[ie] = sl.triag.area(lon[elem[ie,]],lat[elem[ie,]])
		}
		for (i in 1:N) {
			cellareas[i] = sum(elemareas[neighelems[i,]],na.rm=TRUE)
		}
		cellareas = cellareas / 3 * Rearth^2
		if (verbose) {print("... done.")}
	}
	
	return(list(N=N,Nlev=Nlev,N3D=N3D,lon=lon,lat=lat,elem=elem,coast=coast,neighnodes=neighnodes,neighelems=neighelems,stamppoly.lon=stampmat.lon,stamppoly.lat=stampmat.lat,cellareas=cellareas,elemareas=elemareas,depth=depth,depth.lev=depth.lev,boundary=boundary))
	
}