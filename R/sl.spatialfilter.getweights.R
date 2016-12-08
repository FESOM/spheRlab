sl.spatialfilter.getweights <-
function (lon,lat,neighmat,areas,Rsphere=1,method="gauss",gauss.sigma=2*pi*Rsphere/360,cutoff=3*gauss.sigma) {
	
	if (!( method == "gauss" || method == "lin" || method == "const" )) {stop("'method' must be 'gauss', 'lin', or 'const'")}
	
	src_address = c()
	dst_address = c()
	filter_weights = c()
	N = length(lon)
	
	for (i in 1:N) {
		
		# find neighbourhood
		
		neighhood = i
		extend = TRUE
		dists = 0
		
		while (sum(extend) > 0) {
			
			Nn = length(neighhood)
			
			for (j in (1:Nn)[extend]) {
				
				extend[j] = FALSE
				neighs = neighmat[neighhood[j],]
				neighs = neighs[!is.na(neighs)]
				Nneigh = length(neighs)
				if (Nneigh == 0) {next}
				
				for (k in 1:length(neighs)) {
					
					if (neighs[k] %in% neighhood) {next}
					
					d = sl.gc.dist(lon[c(i,neighs[k])],lat[c(i,neighs[k])],Rsphere)
					
					if (d > cutoff) {next}
					
					neighhood = c(neighhood,neighs[k])
					extend = c(extend,TRUE)
					dists = c(dists,d)
					
				}
				
			}
			
		}
		
		# compute weights
		
		if (method == "gauss") {
			
			w = areas[neighhood] * exp(-(dists/gauss.sigma)^2/2)
			
		} else if (method == "lin") {
			
			w = areas[neighhood] * (1 - (dists/cutoff))
			
		} else if (method == "const") {
			
			w = areas[neighhood]
			
		}
		
		w = w / sum(w)
		
		# attach local results to output vectors
		
		src_address = c(src_address,neighhood)
		dst_address = c(dst_address,rep(i,length(w)))
		filter_weights = c(filter_weights,w)
		
	}
	
	return(list(src_address=src_address,dst_address=dst_address,filter_weights=filter_weights))
	
}
