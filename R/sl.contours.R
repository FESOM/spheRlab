sl.contours <-
function (var=NULL,var.nc=NULL,varid=NULL,levels=0,grd=NULL,na.treatment="none",
          return.edge.info=FALSE,verbose=FALSE,neighmat=NULL,lat=NULL,lon=NULL,elem=NULL) {
	
  node.elems = NULL
  argmiss1 = FALSE
  argmiss2 = FALSE
  derive.neighmat = FALSE
  # the redundant argument structure handled in the following is due to the necessity to ensure backward-compatibility
  if (is.null(grd)) {
    if (is.null(lat) || is.null(lon)) {argmiss1 = TRUE}
	  if (is.null(neighmat)) {
	    if (is.null(elem)) {argmiss2 = TRUE} else {derive.neighmat = TRUE}
	  }
  } else {
    if ("lat" %in% names(grd)) {
      if (!is.null(lat)) {warning("ignoring argument 'lat', using 'grd$lat' instead")}; lat = grd$lat
    } else if (is.null(lat)) {argmiss1=TRUE}
    if ("lon" %in% names(grd)) {
      if (!is.null(lon)) {warning("ignoring argument 'lon', using 'grd$lon' instead")}; lon = grd$lon
    } else if (is.null(lon)) {argmiss1=TRUE}
    if ("neighnodes" %in% names(grd) || !is.null(neighmat)) {
      if ("neighnodes" %in% names(grd)) {
        if (!is.null(neighmat)) {warning("ignoring argument 'neighmat', using 'grd$neighnodes' instead")}; neighmat = grd$neighnodes}
    } else {
      derive.neighmat = TRUE
      if ("elem" %in% names(grd)) {if (!is.null(elem)) {warning("ignoring argument 'elem', using 'grd$elem' instead")}; elem = grd$elem}
      else if (is.null(elem)) {argmiss2=TRUE}
    }
    if ("neighelems" %in% names(grd)) {node.elems = grd$neighelems}
  }
  if (argmiss1) {
    stop("'lat' and 'lon' must be provided, each of them either as list element of 'grd' or as individual argument")
  }
  if (argmiss2) {
    stop("At least one of 'grd$neighnodes', 'neighmat', 'grd$elem', and 'elem' must be provided")
  }
  
  if (derive.neighmat) {
    neigh.res = sl.findneighbours(elem)
    neighmat = neigh.res$neighbour.nodes
    if (is.null(node.elems)) {node.elems = neigh.res$neighbour.elements}
  }
  
  N0 = length(lat)
  if (nrow(neighmat) != N0) {stop("'lat' and 'neighmat' are inconsistent")}
  if (length(lon) != N0) {stop("'lon' and 'lat' are inconsistent")}
  M0 = ncol(neighmat)
  
  if (is.null(node.elems)) {
    # find which elements belong to each node; required to derive whether edges are coastal
    if (verbose) {print("generating node.elems")}
    node.elems = matrix(rep(NA,N0*M0),nrow=N0)
    Nnode.elems = rep(0,N0)
    Nelem = nrow(elem)
    Melem = ncol(elem)
    for (ne in 1:Nelem) {
      for (me in 1:Melem) {
        elemx = elem[ne,me]
        if (is.na(elemx)) {next}
        Nnode.elems[elemx] = Nnode.elems[elemx] + 1
        node.elems[elemx,Nnode.elems[elemx]] = ne
      }
    }
    node.elems = node.elems[,1:max(Nnode.elems)]
  }
	
	if (is.null(var)) {
		if (is.null(var.nc)) {stop("either var or var.nc must be provided")}
	  require(ncdf4)
		ifl = nc_open(var.nc)
		var = ncvar_get(ifl,varid=varid)
		if (length(dim(var)) > 1) {stop("input file has more than one dimension")}
		nc_close(ifl)
	} else {
		if (!is.null(var.nc)) {warning("using var, ignoring var.nc")}
	}
		
	# check for NA values
  N = length(var)
  sum.na = sum(is.na(var))
  if (sum.na > 0) {
    if (sum.na == N) {
      stop("var contains only NAs")
    }
    if (na.treatment == "none") {
      stop("'var' contains NA values and 'na.treatment' is 'none'; consider setting 'na.treatment' to 'cut' or 'fill' and rerun")
    }
    if (na.treatment == "cut") {
      if (verbose) {print(paste0(sum.na," of ",N," values in 'var' are NA and will be cut (neighbours will be treated like boundary/coast) ..."))}
      if (is.null(elem)) {
        if (!is.null(grd) && "elem" %in% names(grd)) {elem = grd$elem}
        else {stop("one of 'grd$elem' and 'elem' is required to cut NA values")}
      }
      grd = sl.grid.reduce(grd = list(elem=elem,neighnodes=neighmat,neighelems=node.elems), remove.points = is.na(var))
      neighmat = grd$neighnodes
      node.elems = grd$neighelems
      var = var[grd$reduce.kept$nodes]
      lat = lat[grd$reduce.kept$nodes]
      lon = lon[grd$reduce.kept$nodes]
      N = length(var)
      if (verbose) {print("cutting completed")}
    } else if (na.treatment == "fill") {
      if (verbose) {print(paste0(sum.na," of ",N," values in 'var' are NA and will be filled using sl.field.fillNA() ..."))}
      var = sl.field.fillNA(num=var, grd=list(neighnodes=neighmat), method="interative.adjacent.mean", verbose=verbose)
      if (anyNA(var)) {var[is.na(var)] = Inf}
      if (verbose) {print("filling completed")}
    } else {stop("'var' contains NA values and 'na.treatment' is not valid (must be one of 'none', 'cut', and 'fill')")}
  }
	
	contours.list = list()
	if (dim(neighmat)[1] != N) {stop("'var' and grid are inconsistent")}
	M = dim(neighmat)[2]
	
	Nneigh = rowSums(!is.na(neighmat))
	
	i.max = 20
	
	for (c in 1:length(levels)) {
	
		lev = levels[c]
		if (verbose) {print(paste("computing contour line(s) for level",lev))}
		segments = list()
		s = 0
		
		# check whether field range includes lev
		notinf = (var != Inf)
		if (min(var[notinf]) >= lev || max(var[notinf]) <= lev) {
			contours.list[[c]] = list(level=lev,segments=segments,length=0)
			next
		}
		
		# slightly manipulate values that are exactly equal to lev, if applicable
		exact.matches = which(var == lev)
		l.em = length(exact.matches)
		if (l.em > 0) {
			#print("manipulating values that are exactly equal to lev")
			var.orig = var
			var[exact.matches] = (var[exact.matches] + min(var[var > lev])) / 2
		}
		
		crosses.cont = neighmat
		crosses.cont[!is.na(neighmat)] = FALSE
		crosses.cont.coast = neighmat
		crosses.cont.coast[!is.na(neighmat)] = FALSE
		for (n in 1:N) {
			for (m in 1:M) {
				if (is.na(neighmat[n,m])) {break}
				if (var[n] > lev) {
					if (var[neighmat[n,m]] <= lev) {
						crosses.cont[n,m] = TRUE
						if (sum(!is.na(intersect(node.elems[n,],node.elems[neighmat[n,m],]))) < 2) {
							crosses.cont.coast[n,m] = TRUE
						}
					}
				} else {
					if (var[neighmat[n,m]] > lev) {
						crosses.cont[n,m] = TRUE
						if (sum(!is.na(intersect(node.elems[n,],node.elems[neighmat[n,m],]))) < 2) {
							crosses.cont.coast[n,m] = TRUE
						}
					}
				}
			}
		}
		
		if (l.em > 0) {
			var = var.orig
			rm(var.orig)
		}
		
		edge.done = neighmat * NA
		edge.done[which(crosses.cont==TRUE)] = FALSE
		crosses.cont.count = rowSums(crosses.cont,na.rm=TRUE)
		crosses.cont.coast.count = rowSums(crosses.cont.coast,na.rm=TRUE)
		
		while (sum(crosses.cont.count) > 0) {
			
			# initialize new contour segment
			if (verbose) {print("initialising segment")}
			contour.lat = NULL
			contour.lon = NULL
			if (return.edge.info) {
			  edge.endpoints = NULL
			  edge.relpos = NULL
			}
			s = s + 1
			
			# determine start edge
			if (sum(crosses.cont.coast.count) > 0) {
				n1 = sl.match.comp(0,crosses.cont.coast.count)
				n1n2 = match(TRUE,(crosses.cont.coast[n1,] & !edge.done[n1,]))
				n2 = neighmat[n1,n1n2]
				crosses.cont.coast.count[c(n1,n2)] = crosses.cont.coast.count[c(n1,n2)] - 1
				start.at.coast = TRUE
			} else {
				n1 = sl.match.comp(0,crosses.cont.count)
				n1n2 = match(TRUE,(crosses.cont[n1,] & !edge.done[n1,]))
				n2 = neighmat[n1,n1n2]
				start.at.coast = FALSE
			}
			edge.done[n1,n1n2] = TRUE
			n2n1 = match(n1,neighmat[n2,])
			edge.done[n2,n2n1] = TRUE
			crosses.cont.count[c(n1,n2)] = crosses.cont.count[c(n1,n2)] - 1
			if (var[n2] < var[n1]) {
				n1tmp = n1
				n1 = n2
				n2 = n1tmp
				n1n2tmp = n1n2
				n1n2 = n2n1
				n2n1 = n1n2tmp
			}
			
			# if start at coast, determine direction
			if (start.at.coast) {
				n2.0 = n2
				nnnl.l.inv = n1n2
				last.l = n1
				nnnl.r.inv = n1n2
				last.r = n1
				i = 1
				l.cycle = FALSE
				r.cycle = FALSE
				repeat {
					nnnl.l = ((nnnl.l.inv-2)%%Nneigh[last.l])+1
					next.l = neighmat[last.l,nnnl.l]
					nnnl.r = (nnnl.r.inv%%Nneigh[last.l])+1
					next.r = neighmat[last.r,nnnl.r]
					if (sum(!is.na(intersect(node.elems[next.l,],node.elems[last.l,]))) > 1) {
						if (sum(!is.na(intersect(node.elems[next.r,],node.elems[last.r,]))) > 1) {stop("ill-defined coasts! (e.g. ending  coast line)")}
						cont.dir = -1
						break
					}
					if (sum(!is.na(intersect(node.elems[next.r,],node.elems[last.r,]))) > 1) {
						cont.dir = 1
						break
					}
					if (next.l == n2.0) {l.cycle = TRUE}
					if (next.r == n2.0) {r.cycle = TRUE}
					if (l.cycle && r.cycle) {stop("ill-defined coasts! (single isolated element)")}
					if (i > i.max) {stop(paste("no non-coastal nodes encountered after",i,"iterations"))}
					i = i + 1
					nnnl.l.inv = match(last.l,neighmat[next.l,])
					nnnl.r.inv = match(last.r,neighmat[next.r,])
					last.l = next.l
					last.r = next.r
				}
			} else {
				cont.dir = -1
			}
			cont.dir.m1 = cont.dir - 1
			
			crossfrac = (lev - var[n1])/(var[n2] - var[n1])
			crosspoint = sl.p2p(lon[n1],lat[n1],lon[n2],lat[n2],crossfrac)
			contour.lat = c(contour.lat,crosspoint$lat)
			contour.lon = c(contour.lon,crosspoint$lon)
			if (return.edge.info) {
			  edge.endpoints = rbind(edge.endpoints,c(n1,n2))
			  edge.relpos = c(edge.relpos,crossfrac)
			}
				
			n1.0 = n1
			n2.0 = n2
			
			# complete contour line
			repeat {
				
				n2n1 = n1n2
				
				repeat {
					
					n1n2 = ((n2n1+cont.dir.m1)%%Nneigh[n1])+1
					n2 = neighmat[n1,n1n2]
					n2n1 = match(n1,neighmat[n2,])
					
					if (crosses.cont[n1,n1n2]) {
						break
					}
					
					n1 = n2
					
				}
				
				crossfrac = (lev - var[n1])/(var[n2] - var[n1])
				crosspoint = sl.p2p(lon[n1],lat[n1],lon[n2],lat[n2],crossfrac)
				contour.lat = c(contour.lat,crosspoint$lat)
				contour.lon = c(contour.lon,crosspoint$lon)
				if (return.edge.info) {
				  edge.endpoints = rbind(edge.endpoints,c(n1,n2))
				  edge.relpos = c(edge.relpos,crossfrac)
				}
				
				if (edge.done[n1,n1n2]) {
					if (!((n1 == n1.0 && n2 == n2.0) || (n1 == n2.0 && n2 == n1.0))) {
						stop("edge already done but not equal to starting edge")
						#return(list(segments,contour.lat,contour.lon,n1,n2,neighmat,n1.0,n2.0))
					}
					break
				} else {
					edge.done[n1,n1n2] = TRUE
					edge.done[n2,n2n1] = TRUE
					crosses.cont.count[c(n1,n2)] = crosses.cont.count[c(n1,n2)] - 1
				}
				
				if (crosses.cont.coast[n1,n1n2]) {
					crosses.cont.coast.count[c(n1,n2)] = crosses.cont.coast.count[c(n1,n2)] - 1
					if (!start.at.coast) {stop("contour line started internally but ended at the coast")}
					break
				}
				
			}
			
			if (cont.dir == -1) {
				contour.lat = contour.lat[length(contour.lat):1]
				contour.lon = contour.lon[length(contour.lon):1]
				if (return.edge.info) {
				  edge.endpoints = edge.endpoints[length(contour.lon):1,]
				  edge.relpos = edge.relpos[length(contour.lon):1]
				}
			}
			
			#compute segment length
			segment.length = 0
			for (i in 2:length(contour.lat)) {
				segment.length = segment.length + sl.gc.dist(contour.lon[(i-1):i],contour.lat[(i-1):i])
			}
			
			if (return.edge.info) {
			  segments[[s]] = list(lat=contour.lat,lon=contour.lon,edge.endpoints=edge.endpoints,edge.relpos=edge.relpos,length=segment.length)
			} else {
			  segments[[s]] = list(lat=contour.lat,lon=contour.lon,length=segment.length)
			}
			
		}
		
		contour.length = 0
		for (i in 1:length(segments)) {
			contour.length = contour.length + segments[[i]]$length
		}
		
		contours.list[[c]] = list(level=lev,segments=segments,length=contour.length)
		
	}   # end of loop over contour levels
	
	return(contours.list)
	
}
