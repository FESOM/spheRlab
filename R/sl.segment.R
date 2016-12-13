sl.segment <-
function (logivec,extend=FALSE,first.only=FALSE,return.segments=TRUE) {
	
	L = length(logivec)
	
	f2t = which(!logivec & logivec[c(2:L,1)])
	t2f = which(logivec & !logivec[c(2:L,1)])
	
	if (extend) {
		t2f = (t2f%%L) + 1
	} else {
		f2t = (f2t%%L) + 1
	}
	N.seg = length(t2f)
	
	if (N.seg > 1 && logivec[1]) {t2f = t2f[c(2:N.seg,1)]}
	
	if (return.segments) {
		if (t2f[1] <= f2t[1]) {
			seg = c(f2t[1]:L,1:t2f[1])
		} else {
			seg = f2t[1]:t2f[1]
		}
		if (first.only || N.seg == 1) {
			return(seg)
		} else {
			segments = list(seg)
			for (i in 2:N.seg) {
				if (t2f[i] <= f2t[i]) {
					segments[[i]] = c(f2t[i]:L,1:t2f[i])
				} else {
					segments[[i]] = f2t[i]:t2f[i]
				}
			}
			return(segments)
		}
	} else {
		if (first.only) {
			return(list(start=f2t[1],end=t2f[1]))
		} else {
			return(list(start=f2t,end=t2f))
		}
		
	}
	
}
