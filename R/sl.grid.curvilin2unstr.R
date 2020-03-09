sl.grid.curvilin2unstr <-
function (lon=NULL,lat=NULL,Nx=NULL,Ny=NULL,vars=NULL,neighnodes=TRUE,neighelems=TRUE,quad2triag=TRUE,quad2triag.mode="zigzag",transpose=FALSE,
          cut.top=0,cut.bottom=0,cut.left=0,cut.right=0,close.sides=FALSE,close.top=FALSE,close.bottom=FALSE) {
	
	elem.arg = TRUE
	elem = NULL
	neighnodes.arg = neighnodes
	neighnodes = NULL
	neighelems.arg = neighelems
	neighelems = NULL
	curvilin2unstr.fun = NULL
	
	if (!quad2triag) {stop("only 'quad2triag=TRUE' implemented so far")}
	if (quad2triag && quad2triag.mode!="zigzag") {stop("only 'quad2triag.mode='zigzag'' implemented so far")}
	
	if (!is.null(lon)) {
		if (!is.null(Nx)) {warning("deriving 'Nx' from 'lon', ignoring argument 'Nx'")}
		if (length(sl.dim(lon))==1) {
			Nx = length(lon)
			if (length(sl.dim(lat))!=1) {stop("given that 'lon' is a vector, 'lat' must be a vector as well")}
			if (!is.null(Ny)) {warning("deriving 'Ny' from 'lat', ignoring argument 'Ny'")}
			Ny = length(lat)
			lon = rep(lon,Ny)
			lat0 = lat
			lat = c()
			for (i in 1:Ny) {lat = c(lat,rep(lat0[i],Nx))}
		} else if (length(sl.dim(lon))==2) {
		  if (!identical(sl.dim(lat), sl.dim(lon))) {stop("given that 'lon' is a matrix, 'lat' must have the same shape")}
		  if (transpose) {
		    Nx0 = nrow(lon)
		    if (!is.null(Ny)) {"deriving 'Ny' from 'lon', ignoring argument 'Ny'"}
		    Ny0 = ncol(lon)
		    curvilin2unstr.fun = function (x) {return(as.vector(x[(1+cut.left):(Nx0-cut.right),(1+cut.top):(Ny0-cut.bottom)]))}
		  } else {
		    Nx0 = ncol(lon)
		    if (!is.null(Ny)) {"deriving 'Ny' from 'lon', ignoring argument 'Ny'"}
		    Ny0 = nrow(lon)
		    curvilin2unstr.fun = function (x) {return(as.vector(t(x)[(1+cut.left):(Nx0-cut.right),(1+cut.top):(Ny0-cut.bottom)]))}
		  }
		  Nx = Nx0 - cut.left - cut.right
		  Ny = Ny0 - cut.top - cut.bottom
			lon = curvilin2unstr.fun(lon)
			lat = curvilin2unstr.fun(lat)
		} else {stop("'lon' must be a vector or a matrix")}
	} else {
		if (is.null(Nx) || is.null(Ny)) {stop("given that 'lon' is not specified, both 'Nx' and 'Ny' must be specified")}
	}
	if (Nx < 3) {stop("length of x-coordinate must be >= 3")}
	if (Ny < 2) {stop("length of y-coordinate must be >= 2")}
	N = Nx * Ny
	
	# defining neighbourhood
	neigh.right = 2:(N+1)
	neigh.left = 0:(N-1)
	neigh.top = c(rep(NA,Nx),1:(N-Nx))
	if (close.top) {
	  if (Nx%%2 != 0) {stop("top boundary can not be closed with uneven number of columns")}
	  neigh.top[1:(Nx/2-1)] = Nx:(Nx/2+2)
	  neigh.top[Nx:(Nx/2+2)] = 1:(Nx/2-1)
	  if (close.sides) {neigh.top[c(1,Nx)] = NA}
	}
	neigh.bottom = c((Nx+1):N,rep(NA,Nx))
	if (close.bottom) {
	  if (Nx%%2 != 0) {stop("bottom boundary can not be closed with uneven number of columns")}
	  neigh.bottom[(N-Nx+1):(N-Nx/2-1)] = N:(N-Nx/2+2)
	  neigh.bottom[N:(N-Nx/2+2)] = (N-Nx+1):(N-Nx/2-1)
	  if (close.sides) {neigh.bottom[c(N-Nx+1,N)] = NA}
	}
	if (quad2triag) {
		neigh.topright = c(rep(NA,Nx),2:(N-Nx+1))
		neigh.topleft = c(rep(NA,Nx),0:(N-Nx-1))
		neigh.bottomright = c((Nx+2):(N+1),rep(NA,Nx))
		neigh.bottomleft = c(Nx:(N-1),rep(NA,Nx))
		if (close.top) {
		  neigh.topright[1:(Nx/2-1)] = (Nx-1):(Nx/2+1)
		  neigh.topright[(Nx/2+1):(Nx-1)] = (Nx/2-1):1
		  neigh.topleft[2:(Nx/2)] = Nx:(Nx/2+2)
		  neigh.topleft[(Nx/2+2):Nx] = (Nx/2):2
		}
		if (close.bottom) {
		  neigh.bottomright[(N-Nx)+(1:(Nx/2-1))] = (N-Nx)+((Nx-1):(Nx/2+1))
		  neigh.bottomright[(N-Nx)+((Nx/2+1):(Nx-1))] = (N-Nx)+((Nx/2-1):1)
		  neigh.bottomleft[(N-Nx)+(2:(Nx/2))] = (N-Nx)+(Nx:(Nx/2+2))
		  neigh.bottomleft[(N-Nx)+((Nx/2+2):Nx)] = (N-Nx)+((Nx/2):2)
		}
	}
	if (close.sides) {
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
	
	#neighnodes2elem
	if (elem.arg) {
		if (quad2triag) {
			Ne = 2*N
			elem = matrix(nrow=Ne,ncol=3)
			ne = 0
			if (close.top) {
			  for (n in 1:Nx) {
			    for (nn in 5:8) {
			      if (!anyNA(neighnodes[n,c(nn,(nn%%8)+1)])) {
			        ne = ne + 1
			        elem[ne,] = c(n,neighnodes[n,c(nn,(nn%%8)+1)])
			        if (n > Nx/2) {
			          if (any(apply(elem[1:(ne-1),],1,function(x){all(elem[ne,]%in%x)}))) {
			            elem[ne,] = NA
			            ne = ne - 1
			          }
			        }
			      }
			    }
			  }
			}
			for (n in 1:(N-Nx)) {
				for (nn in 1:4) {
					if (!anyNA(neighnodes[n,c(nn,nn+1)])) {
						ne = ne + 1
						elem[ne,] = c(n,neighnodes[n,c(nn,nn+1)])
					}
				}
			}
			if (close.bottom) {
			  ne.nobot = ne
			  for (n in (N-Nx+1):N) {
			    for (nn in 1:4) {
			      if (!anyNA(neighnodes[n,c(nn,nn+1)])) {
			        ne = ne + 1
			        elem[ne,] = c(n,neighnodes[n,c(nn,nn+1)])
			        if (n > (N-Nx/2)) {
			          if (any(apply(elem[(ne.nobot+1):(ne-1),],1,function(x){all(elem[ne,]%in%x)}))) {
			            elem[ne,] = NA
			            ne = ne - 1
			          }
			        }
			      }
			    }
			  }
			}
		} else {
			stop("'elem=TRUE' not yet implemented for 'quad2triag=FALSE'")
			Ne = N - Nx/2
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
	
	if (neighnodes.arg || neighelems.arg) {
	  findneighbours.res = sl.findneighbours(elem = elem)
	  if (neighnodes.arg) {
	    neighnodes = findneighbours.res$neighbour.nodes
	  } else {neighnodes = NULL}
	  if (neighelems.arg) {
	    neighelems = findneighbours.res$neighbour.elems
	  }
	}
	
	if (!is.null(vars)) {
	  for (i in 1:length(vars)) {
	    if (sl.dim(vars[[i]]) == 2) {
	      vars[[i]] = curvilin2unstr.fun(vars[[i]])
	    }
	  }
	}
	
	return(list(lon=lon,lat=lat,elem=elem,coast=NULL,openbound=NULL,neighnodes=neighnodes,neighelems=neighelems,vars=vars,curvilin2unstr.fun=curvilin2unstr.fun))
	
}
