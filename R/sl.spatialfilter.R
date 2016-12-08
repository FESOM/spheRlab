sl.spatialfilter <-
function (num,sf.gw.res) {
	
	w = sf.gw.res$filter_weights
	s = sf.gw.res$src_address
	d = sf.gw.res$dst_address
	N = length(num)
	Ni = length(w)
	
	num.filtered = rep(NA,N)
	i = 1
	for (d.ind in 1:N) {
		
		nu = 0
		while (i <= Ni && d[i] == d.ind) {
			nu = nu + w[i]*num[s[i]]
			i = i + 1
		}
		num.filtered[d.ind] = nu
		
	}
	
	return(num.filtered)
	
}
