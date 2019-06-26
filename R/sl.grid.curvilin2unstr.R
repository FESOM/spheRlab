sl.grid.curvilin2unstr <-
function (lon=NULL,lat=NULL,Nx=NULL,Ny=NULL,vars=NULL,byrow=TRUE,x.swap=FALSE,y.swap=FALSE,neighnodes=TRUE,elem=TRUE,quad2triag=TRUE,quad2triag.mode="zigzag",close.north=FALSE,close.south=FALSE,close.ns.triags=TRUE,close.zonal=TRUE,landseamask=NULL) {
	
	elem.arg = elem
	elem = NULL
	neighnodes.arg = neighnodes
	neighnodes = NULL
	
	#if (!quad2triag) {stop("only 'quad2triag=TRUE' implemented so far")}
	if (quad2triag && quad2triag.mode!="zigzag") {stop("only 'quad2triag.mode='zigzag'' implemented so far")}
	if (!is.null(landseamask)) {stop("using a 'landseamask' not yet implemented; must be set to NULL")}
	if (close.north || close.south) {"closing north and south boundaries (encompassing poles) not yet implemented"}
	if (!byrow) {stop("only 'byrow=TRUE' implemented so far")}
	
	if (!is.null(lon)) {
		if (!is.null(Nx)) {warning("deriving 'Nx' from 'lon', ignoring argument 'Nx'")}
		if (length(sl.dim(lon))==1) {
			Nx = length(lon)
			if (length(sl.dim(lat))!=1) {stop("given that 'lon' is a vector, 'lat' must be a vector as well")}
			if (!is.null(Ny)) {warning("deriving 'Ny' from 'lat', ignoring argument 'Ny'")}
			Ny = length(lat)
			if (byrow) {
				lon = rep(lon,Ny)
				lat0 = lat
				lat = c()
				for (i in 1:Ny) {lat = c(lat,rep(lat0[i],Nx))}
			} else {
				stop("ups")
			}
		} else if (length(sl.dim(lon))==2) {
		  if (!identical(sl.dim(lat), sl.dim(lon))) {stop("given that 'lon' is a matrix, 'lat' must have the same shape")}
			Nx = ncol(lon)
			if (!is.null(Ny)) {"deriving 'Ny' from 'lon', ignoring argument 'Ny'"}
			Ny = nrow(lon)
			lon = as.vector(t(lon))
			lat = as.vector(t(lat))
			if (!is.null(lat)) {lat = as.vector(t(lat))}
		} else {stop("'lon' must be a vector or a matrix")}
	} else {
		if (is.null(Nx) || is.null(Ny)) {stop("given that 'lon' is not specified, both 'Nx' and 'Ny' must be specified")}
	}
	if (Nx < 3) {stop("length of x-coordinate (number of 'longitudes') must be >= 3")}
	if (Ny < 2) {stop("length of y-coordinate (number of 'latitudes') must be >= 2")}
	N = Nx * Ny
	
	# defining neighbourhood
	neigh.right = 2:(N+1)
	neigh.left = 0:(N-1)
	neigh.top = c(rep(NA,Nx),1:(N-Nx))
	neigh.bottom = c((Nx+1):N,rep(NA,Nx))
	if (quad2triag) {
		neigh.topright = c(rep(NA,Nx),2:(N-Nx+1))
		neigh.topleft = c(rep(NA,Nx),0:(N-Nx-1))
		neigh.bottomright = c((Nx+2):(N+1),rep(NA,Nx))
		neigh.bottomleft = c(Nx:(N-1),rep(NA,Nx))
	}
	if (close.zonal) {
		neigh.right[seq(Nx,N,Nx)] = seq(1,N,Nx)
		neigh.left[seq(1,N,Nx)] = seq(Nx,N,Nx)
		if (quad2triag) {
			neigh.topright[seq(2*Nx,N,Nx)] = seq(1,N-Nx,Nx)
			neigh.topleft[seq(Nx+1,N,Nx)] = seq(Nx,N-Nx,Nx)
			neigh.bottomright[seq(Nx,N-Nx,Nx)] = seq(Nx+1,N,Nx)
			neigh.bottomleft[seq(1,N-Nx,Nx)] = seq(2*Nx,N,Nx)
		}
	} else {
		neigh.right[seq(Nx,N,Nx)] = NA
		neigh.left[seq(1,N,Nx)] = NA
		if (quad2triag) {
			neigh.topright[seq(2*Nx,N,Nx)] = NA
			neigh.topleft[seq(Nx+1,N,Nx)] = NA
			neigh.bottomright[seq(Nx,N-Nx,Nx)] = NA
			neigh.bottomleft[seq(1,N-Nx,Nx)] = NA
		}
	}
	if (quad2triag) {
		if (quad2triag.mode == "zigzag") {
			for (ny in 0:(Ny-1)) {
				neigh.topright[seq(Nx*ny+1,Nx*ny+Nx,2)] = NA
				neigh.topleft[seq(Nx*ny+3,Nx*ny+Nx,2)] = NA
				neigh.bottomright[seq(Nx*ny+2,Nx*ny+Nx,2)] = NA
				neigh.bottomleft[seq(Nx*ny+2,Nx*ny+Nx,2)] = NA
			}
			if (Nx%%2 == 0) {
				neigh.topleft[seq(1,N,Nx)] = NA
			} else {
				neigh.bottomleft[seq(1,N,Nx)] = NA
			}
		} else {
			stop("ups")
		}
	}
	
	if (quad2triag) {
		maxneigh = 8
		neighnodes = matrix(nrow=N,ncol=maxneigh)
		neighnodes[,1] = neigh.left
		neighnodes[,2] = neigh.bottomleft
		neighnodes[,3] = neigh.bottom
		neighnodes[,4] = neigh.bottomright
		neighnodes[,5] = neigh.right
		neighnodes[,6] = neigh.topright
		neighnodes[,7] = neigh.top
		neighnodes[,8] = neigh.topleft
	} else {
		maxneigh = 4
		neighnodes = matrix(nrow=N,ncol=maxneigh)
		neighnodes[,1] = neigh.left
		neighnodes[,2] = neigh.bottom
		neighnodes[,3] = neigh.right
		neighnodes[,4] = neigh.top
	}
	Nneighs = apply(neighnodes,1,function(x) sum(!is.na(x)))
	
	#if ((close.north || close.south) && close.ns.triags)
	#	maxneigh = max(maxneigh,1+ceiling(log(Nx,base=2)))   # not correct ...
	#}
	
	#neighnodes2elem
	if (elem.arg) {
		if (quad2triag) {
			Ne = 2*(N-Nx)
			elem = matrix(nrow=Ne,ncol=3)
			ne = 0
			for (n in 1:N) {
				for (nn in 1:4) {
					if (!anyNA(neighnodes[n,c(nn,nn+1)])) {
						ne = ne + 1
						elem[ne,] = c(n,neighnodes[n,c(nn,nn+1)])
					}
				}
			}
		} else {
			stop("'elem=TRUE' not yet implemented for 'quad2triag=FALSE'")
			Ne = N - Nx
			elem = matrix(nrow=Ne,ncol=4)
			ne = 0
			for (n in 1:N) {
				if (!anyNA(neighnodes[n,1:2])) {
					ne = ne + 1
					elem[ne,] = c(n,neighnodes[n,c(nn,nn+1)]) # fails to include fourth point into elements (diagonal)
				}
			}
		}
	}
	
	# shift gaps (NAs) in each row of neighnodes to the right.
	# didn't do this earlier because derivation of elem depends on the column meaning!
	for (n in 1:N) {
		if (Nneighs[n] < maxneigh) {
			neighnodes[n,1:Nneighs[n]] = neighnodes[n,which(!is.na(neighnodes[n,]))]
			neighnodes[n,(Nneighs[n]+1):maxneigh] = NA
		}
	}
	
	elem.N.notna = rowSums(!is.na(elem))
	if (any(c(1,2) %in% elem.N.notna)) {warning("grid contains elements with only one or two vertices, something went wrong")}
	elem = elem[elem.N.notna > 0, ]
	
	return(list(lon=lon,lat=lat,elem=elem,coast=NULL,openbound=NULL,neighnodes=neighnodes,neighelems=NULL,vars=vars))
	
}
