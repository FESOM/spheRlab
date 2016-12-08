sl.elem2linepairs <-
function(elem,concat=TRUE,verbose=FALSE) {
	
	Ne = nrow(elem)
	if (Ne == 3) {
		warning("number of elements appears to be only 3; check if dimensions in elem are correct")
	}
	Ne3 = Ne * 3
	pairs = matrix(ncol=2,nrow=Ne3)
	pairs[seq(1,Ne3-2,3),] = elem[,c(1,2)]
	pairs[seq(2,Ne3-1,3),] = elem[,c(2,3)]
	pairs[seq(3,Ne3,3),] = elem[,c(3,1)]
	pairs = t(apply(pairs,1,sort))
	pairs = pairs[order(pairs[,1],pairs[,2]),]
	keep = rep(TRUE,Ne3)
	i = 1
	while (i < Ne3) {
		if (identical(pairs[i,],pairs[i+1,])) {
			keep[i+1] = FALSE
			i = i + 2
		} else {
			i = i + 1
		}
	}
	
	pairs.unique = pairs[keep,]
	
	if (concat) {
		pu1 = pairs.unique[,1]
		pu2 = pairs.unique[,2]
		Npairs = length(pu1)
		pairs.concat = rep(NA,3*Npairs)
		nm = 1
		n.tail = 2
		pairs.concat[(n.tail-1):n.tail] = c(pu1[nm],pu2[nm])
		pu1[nm] = NA
		pu2[nm] = NA
		node.last = pairs.concat[n.tail]
		c = 1
		if (verbose) {print(paste("concatenating",Npairs,"line pairs"))}
		while (c < Npairs) {
			nm = match(node.last,pu1)
			if (!is.na(nm)) {
				n.tail = n.tail + 1
				node.last = pu2[nm]
				pairs.concat[n.tail] = node.last
			} else {
				nm = match(node.last,pu2)
				if (!is.na(nm)) {
					n.tail = n.tail + 1
					node.last = pu1[nm]
					pairs.concat[n.tail] = node.last
				} else {
					if (verbose) {print(paste(c,"line pairs completed, starting new segment"))}
					nm = which(!is.na(pu1))[1]
					n.tail = n.tail + 3
					node.last = pu2[nm]
					pairs.concat[(n.tail-1):n.tail] = c(pu1[nm],pu2[nm])
				}
			}
			pu1[nm] = NA
			pu2[nm] = NA
			c = c + 1
		}
		not.na = which(!is.na(pairs.concat))
		return(pairs.concat[1:not.na[length(not.na)]])
	} else {
		return(pairs.unique)
	}

}
