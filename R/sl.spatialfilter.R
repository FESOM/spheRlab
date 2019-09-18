sl.spatialfilter <-
function (num,sf.gw.res,na.rm=FALSE) {
	
	w = sf.gw.res$filter_weights
	s = sf.gw.res$src_address
	d = sf.gw.res$dst_address
	N = length(num)
	Ni = length(w)
	
	num.filtered = rep(NA,N)
	for (d.ind in 1:N) {
		
	  inds.tmp = which(d==d.ind)
	  num.tmp = num[s[inds.tmp]]
	  w.tmp = w[inds.tmp]
		if (na.rm) {
		  na.tmp = is.na(num.tmp)
		  if (sum(na.tmp) > 0) {
		    if (sum(na.tmp) == length(inds.tmp)) {next}
		    w.tmp.sum = sum(w.tmp[!na.tmp])
		    if (w.tmp.sum == 0) {next}
		    w.tmp = w.tmp / w.tmp.sum
		    num.tmp[na.tmp] = 0
		  }
		}
	  num.filtered[d.ind] = sum(w.tmp*num.tmp)
		
	}
	
	return(num.filtered)
	
}
