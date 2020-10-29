sl.grid.mergepoints <-
function (grd, remove.points, merge.points, return.info = TRUE, auto.reduce = TRUE) {
	
	elem = grd$elem
	if (is.null(elem)) {stop("'grd' must contain a list-element named 'elem'")}
	N = max(elem)
	Nelem = nrow(elem)
	Nrp = length(remove.points)
	if (length(merge.points) != Nrp) {stop("'merge.points' and 'remove.points' must have the same length")}
	if (any(duplicated(c(remove.points,merge.points)))) {
	  stop("'merge.points' and 'remove.points' must not contain any duplicates")
	}
	
	do.coast = FALSE
	if (!is.null(grd$coast)) {
	  do.coast = TRUE
	  coast = grd$coast
	}
	do.openbound = FALSE
	if (!is.null(grd$openbound)) {
	  do.openbound = TRUE
	  openbound = grd$openbound
	}
	
	neighnodes = grd$neighnodes
	if (is.null(neighnodes)) {stop("'grd' must contain a list-element named 'neighnodes'; consider using sl.findneighbours() first")}
	if (nrow(neighnodes) != N) {stop("Number of rows in 'grd$neighnodes' inconsistent with 'max(grd$elem)'")}
	max.nn = ncol(neighnodes)
	# double the number of columns (fill with NAs) to be on the safe side when rows need to be extended
	neighnodes = cbind(neighnodes,matrix(ncol=max.nn,nrow=N))
	neighelems = grd$neighelems
	if (is.null(neighelems)) {stop("'grd' must contain a list-element named 'neighelems'; consider using sl.findneighbours() first")}
	if (nrow(neighelems) != N) {stop("Number of rows in 'grd$neighelems' inconsistent with 'max(grd$elem)'")}
	max.ne = ncol(neighelems)
	# double the number of columns (fill with NAs) to be on the safe side when rows need to be extended
	neighelems = cbind(neighelems,matrix(ncol=max.ne,nrow=N))
	
	nodes.affected = NULL
	remove.elems = NULL
	for (i in 1:length(remove.points)) {
	  
	  rpi = remove.points[i]
	  mpi = merge.points[i]
	  nodes.affected.i = neighnodes[rpi,]
	  nodes.affected.i = nodes.affected.i[!is.na(nodes.affected.i)]
	  nodes.affected = c(nodes.affected, nodes.affected.i)
	  
	  # replace index of obsolete point in elem by index of the corresponding merge point
	  elem[elem == rpi] = mpi
	  
	  # identify elements that must be removed
	  remove.elems.i = which(rowSums(elem == mpi) > 1)
	  remove.elems = c(remove.elems, remove.elems.i)
	  
	  # merge non-removed neighelems of obsolete point into neighelems of merge point, keeping order intact
	  if (length(remove.elems.i) > 0) {
	    mpi2rei = which(neighelems[mpi,] %in% remove.elems.i)
	    rpi2rei = which(neighelems[rpi,] %in% remove.elems.i)
	    neighelems.insert.which = which(!(neighelems[rpi,] %in% remove.elems.i | is.na(neighelems[rpi,])))
	    Ninsert = length(neighelems.insert.which)
	    if (Ninsert > 0) {
	      if (Ninsert > 1 && max(neighelems.insert.which)-min(neighelems.insert.which)+1 > Ninsert) {
	        if (length(rpi2rei) > 1 && rpi2rei[2] - rpi2rei[1] != 1) {stop("this should not happen")}
	        neighelems.insert = neighelems[rpi,c(neighelems.insert.which[neighelems.insert.which > max(rpi2rei)],
	                                             neighelems.insert.which[neighelems.insert.which < min(rpi2rei)])]
	      } else {
	        neighelems.insert = neighelems[rpi,neighelems.insert.which]
	      }
	      Nneigh = sum(!is.na(neighelems[mpi,]))
	      neighelems.extended = NULL
	      if (min(mpi2rei) > 1) {neighelems.extended = c(neighelems.extended, neighelems[mpi,1:(min(mpi2rei)-1)])}
	      neighelems.extended = c(neighelems.extended, neighelems.insert)
	      if (max(mpi2rei) < Nneigh) {neighelems.extended = c(neighelems.extended, neighelems[mpi,(max(mpi2rei)+1):Nneigh])}
	      neighelems[mpi,1:length(neighelems.extended)] = neighelems.extended
	    }
	  } else {
	    warning(paste0("Points of pair #",i," (index ",rpi," and ",mpi,") do not have any shared elements, resulting element order might be wrong"))
	    neighelems.extended = c(neighelems[mpi,which(!is.na(neighelems[mpi,]))], neighelems[rpi,which(!is.na(neighelems[rpi,]))])
	    neighelems[mpi,1:length(neighelems.extended)] = neighelems.extended
	  }
	  
	  # merge non-redundant neighnodes of obsolete point into neighnodes of merge point, keeping order intact
	  mpi2rpi = which(neighnodes[mpi,] == rpi)
	  if (length(mpi2rpi) == 0) {stop(paste0("Points of pair #",i," (index ",rpi," and ",mpi,") are not neighbours"))}
	  rpi2mpi = which(neighnodes[rpi,] == mpi)
	  neighnodes.insert.which = which(!(neighnodes[rpi,] %in% c(mpi,neighnodes[mpi,]) | is.na(neighnodes[rpi,])))
	  if (length(neighnodes.insert.which) > 0) {
	    neighnodes.insert = neighnodes[rpi,c(neighnodes.insert.which[neighnodes.insert.which > rpi2mpi],
	                                         neighnodes.insert.which[neighnodes.insert.which < rpi2mpi])]
	    Nneigh = sum(!is.na(neighnodes[mpi,]))
	    neighnodes.extended = NULL
	    if (mpi2rpi > 1) {neighnodes.extended = c(neighnodes.extended, neighnodes[mpi,1:(mpi2rpi-1)])}
	    neighnodes.extended = c(neighnodes.extended, neighnodes.insert)
	    if (mpi2rpi < Nneigh) {neighnodes.extended = c(neighnodes.extended, neighnodes[mpi,(mpi2rpi+1):Nneigh])}
	    neighnodes[mpi,1:length(neighnodes.extended)] = neighnodes.extended
	  }
	  
	  for (j in nodes.affected.i) {
	    
	    # remove removed element(s) from neighelems
	    neighelems.reduced = neighelems[j,]
	    neighelems.reduced[neighelems.reduced %in% remove.elems.i] = NA
	    neighelems.reduced = neighelems.reduced[!is.na(neighelems.reduced)]
	    neighelems[j, ] = NA
	    neighelems[j, 1:length(neighelems.reduced)] = neighelems.reduced
	    
	    # remove removed point from neighnodes, or replace index of obsolete point by index of the corresponding merge point
	    neighnodes.reduced = neighnodes[j,]
	    neighnodes.reduced = neighnodes.reduced[!is.na(neighnodes.reduced)]
	    neighnodes[j, ] = NA
	    if (mpi %in% neighnodes.reduced) {
	      neighnodes.reduced[neighnodes.reduced == rpi] = NA
	      neighnodes.reduced = neighnodes.reduced[!is.na(neighnodes.reduced)]
	    } else {
	      neighnodes.reduced[neighnodes.reduced == rpi] = mpi
	    }
	    neighnodes[j, 1:length(neighnodes.reduced)] = neighnodes.reduced
	  }
	  
	  # transfer coast / openbound flag from removed point to merge point
	  if (do.coast && coast[rpi]) {coast[mpi] = TRUE}
	  if (do.openbound && openbound[rpi]) {openbound[mpi] = TRUE}
	  
	}
	
	keep.points = !(1:N %in% remove.points)
	N.keep = sum(keep.points)
	neighnodes = neighnodes[keep.points, ]
	keep.elems = !(1:Nelem %in% remove.elems)
	Nelem.keep = sum(keep.elems)
	neighelems = neighelems[keep.points, ]
	neighelems.orig = neighelems
	
	# delete rightmost column(s), should they be NA-only
	Nneel.max.new = max(rowSums(!is.na(neighelems)))
	neighelems = neighelems[,1:Nneel.max.new]
	if (Nneel.max.new > max.ne) {
	  warning(paste("extended 'neighelems' matrix from",max.ne,"to",Nneel.max.new,"columns"))
	}
	Nneno.max.new = max(rowSums(!is.na(neighnodes)))
	neighnodes = neighnodes[,1:Nneno.max.new]
	if (Nneno.max.new > max.nn) {
	  warning(paste("extended 'neighnodes' matrix from",max.nn,"to",Nneno.max.new,"columns"))
	}
	
	elem = elem[keep.elems, ]
	
	# renumbering of the points in elem, neighnodes, and the elements in neighelems
	which.points = which(keep.points)
	if (max(elem) != max(neighnodes,na.rm=TRUE)) {stop("'elem' and 'neighnodes' have different maxima; something went wrong")}
	node.mapping = rep(NA,max(elem))
  node.mapping[which.points] = 1:N.keep
  which.elems = which(keep.elems)
  elem = matrix(node.mapping[elem],ncol=3)
  neighnodes = matrix(node.mapping[neighnodes],ncol=Nneno.max.new)
  elem.mapping = rep(NA,max(neighelems,na.rm=TRUE))
  elem.mapping[which.elems] = 1:Nelem.keep
  neighelems = matrix(elem.mapping[neighelems],ncol=Nneel.max.new)
	
  # prepare return value 'grd'
	grd$elem = elem
	if (do.coast) {grd$coast = coast[keep.points]}
	if (do.openbound) {grd$openbound = openbound[keep.points]}
	grd$neighnodes = neighnodes
	grd$neighelems = neighelems
	
	if (auto.reduce) {
	  if (N == Nelem) {
	    warning(paste("auto detection and reduction of list-elements of",
	                  "'grd' not possible because N(nodes) = N(elems);",
	                  "returning corresponding list-elements as is (if present)"))
	  } else {
	    for (i in which(!(names(grd) %in% c("elem","coast","openbound","neighnodes","neighelems")))) {
	      if (is.vector(grd[[i]])) {
	        if (length(grd[[i]]) == N) {
	          grd[[i]] = grd[[i]][keep.points]
	          warning(paste("reduced list-element",names(grd)[i],"automatically based on vector length = N(nodes)"))
	        }
	        if (length(grd[[i]]) == Nelem) {
	          grd[[i]] = grd[[i]][keep.points]
	          warning(paste("reduced list-element",names(grd)[i],"automatically based on vector length = N(elems)"))
	        }
	      } else {
	        warning(paste("list-element",names(grd)[i],"is not a vector, returning as is"))
	      }
	    }
	  }
	} else {
	  for (i in which(!(names(grd) %in% c("elem","coast","openbound","neighnodes","neighelems")))) {
	    warning(paste("returning list-element",names(grd)[i],"as is (auto.reduce=FALSE)"))
	  }
	}
	
	if (return.info) {
	  grd$merge.kept = list(nodes=which.points, elems=which.elems)
	}
	
	return(grd)

}
