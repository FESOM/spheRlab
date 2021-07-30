sl.grid.reduce <-
function (grd, remove.points, set.coast = TRUE, set.openbound = FALSE, return.info = TRUE, auto.reduce = TRUE) {
	
	elem = grd$elem
	if (is.null(elem)) {stop("'grd' must contain a list-element named 'elem'")}
	N = max(elem)
	Nelem = nrow(elem)
	coast = grd$coast
	if (set.coast && is.null(coast)) {
	  coast = rep(FALSE,N)
	}
	openbound = grd$openbound
	if (set.openbound && is.null(openbound)) {
	  openbound = rep(FALSE,N)
	}
	neighnodes = grd$neighnodes
	if (is.null(neighnodes)) {stop("'grd' must contain a list-element named 'neighnodes'; consider using sl.findneighbours() first")}
	neighelems = grd$neighelems
	if (is.null(neighelems)) {stop("'grd' must contain a list-element named 'neighelems'; consider using sl.findneighbours() first")}
	
	if (!is.logical(remove.points)) {
	  rlp = rep(FALSE,N)
	  rlp[remove.points] = TRUE
	  remove.points = rlp
	}
	keep.points = !remove.points
	remove.points.orig = remove.points
	
	N.keep = sum(keep.points)
	keep.elems = rep(TRUE,Nelem)
	
	for (i in which(remove.points)) {
	  rm.elems = neighelems[i,!is.na(neighelems[i,])]
	  keep.elems[rm.elems] = FALSE
	  if (set.coast) {
	    coast[unique(as.vector(elem[rm.elems, ]))] = TRUE
	  }
	  if (set.openbound) {
	    openbound[unique(as.vector(elem[rm.elems, ]))] = TRUE
	  }
	}
	Nelem.keep = sum(keep.elems)
	
	for (i in which(keep.points)) {
	  neighelems.i = neighelems[i,!is.na(neighelems[i,])]
	  if (sum(keep.elems[neighelems.i]) == 0) {
	    keep.points[i] = FALSE
	    remove.points[i] = TRUE
	  }
	}
	if (sum(keep.points) < N.keep) {
	  warning(paste("removed",N.keep-sum(keep.points),"additional grid points without adjacent elements"))
	  N.keep = sum(keep.points)
	}
	
	neighelems = neighelems[keep.points, ]
	neighnodes = neighnodes[keep.points, ]
	neighelems.orig = neighelems
	
	# remove connections to removed elements and nodes
	neighelems[neighelems %in% which(!keep.elems)] = NA
	neighnodes[neighnodes %in% which(remove.points)] = NA
	
	# tidy up the matrices neighelems and neighnodes
	Nneel.max = ncol(neighelems)
	Nneno.max = ncol(neighnodes)
	for (i in 1:N.keep) {
	  
	  # shift non-NA neighbour-element values to the leftmost columns
	  neel = neighelems[i, ]
	  Nneel = sum(!is.na(neel))
	  if (Nneel == 0) {stop("encountered node without neighbouring element, which should not happen")}
	  neighelems[i, 1:Nneel] = neel[!is.na(neel)]
	  if (Nneel.max > Nneel) {neighelems[i, (Nneel+1):Nneel.max] = NA}
	  
	  # shift non-NA neighbour-node values to the leftmost columns
	  neno = neighnodes[i, ]
	  Nneno = sum(!is.na(neno))
	  if (Nneno < 2) {stop("encountered node with less than two neighbouring nodes, which should not happen")}
	  neno = neno[!is.na(neno)]
	  neighnodes[i, 1:Nneno] = neno
	  if (Nneno.max > Nneno) {neighnodes[i, (Nneno+1):Nneno.max] = NA}
	  
	  # delete node connections between nodes without common element remaining
	  neel.orig = neighelems.orig[i, ]
	  neel.orig = neel.orig[!is.na(neel.orig)]
	  if (any(!keep.elems[neel.orig])) {
	    for (j in neno) {
	      if (!(j %in% elem[neel[!is.na(neel)], ])) {
	        j.ind = which(neno == j)
	        neighnodes[i, j.ind] = NA
	        neno = neighnodes[i, !is.na(neighnodes[i, ])]
	        Nneno = Nneno - 1
	        neighnodes[i, 1:Nneno] = neno
	        neighnodes[i, (Nneno+1):Nneno.max] = NA
	      }
	    }
	  }
	  
	}
	
	# delete rightmost column(s), should they be NA-only
	Nneel.max.new = max(rowSums(!is.na(neighelems)))
	if (Nneel.max.new < Nneel.max) {
	  neighelems = neighelems[,1:Nneel.max.new]
	  warning(paste("shrinked 'neighelems' matrix from",Nneel.max,"to",Nneel.max.new,"columns"))
	}
	Nneno.max.new = max(rowSums(!is.na(neighnodes)))
	if (Nneno.max.new < Nneno.max) {
	  neighnodes = neighnodes[,1:Nneno.max.new]
	  warning(paste("shrinked 'neighnodes' matrix from",Nneno.max,"to",Nneno.max.new,"columns"))
	}
	
	elem = elem[keep.elems, ]
	
	# renumbering of the points in elem, neighnodes, and the elements in neighelems
	which.points = which(keep.points)
	if (max(elem) != max(neighnodes,na.rm=TRUE)) {stop("'elem' and 'neighnodes' have different maxima; something went wrong")}
	node.mapping = rep(NA,max(elem))
  node.mapping[which.points] = 1:N.keep
  elem = matrix(node.mapping[elem],ncol=3)
  neighnodes = matrix(node.mapping[neighnodes],ncol=Nneno.max.new)
  elem.mapping = rep(NA,max(neighelems,na.rm=TRUE))
  elem.mapping[which(keep.elems)] = 1:Nelem.keep
  neighelems = matrix(elem.mapping[neighelems],ncol=Nneel.max.new)
	
  # prepare return value 'grd'
	grd$elem = elem
	if (!is.null(coast)) {grd$coast = coast[keep.points]}
	if (!is.null(openbound)) {grd$openbound = openbound[keep.points]}
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
	          grd[[i]] = grd[[i]][keep.elems]
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
	  grd$reduce.kept = list(nodes=which.points, elems=which(keep.elems))
	}
	
	return(grd)

}
