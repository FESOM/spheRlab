sl.checkposition <-
function(a,b,c,ccw.defined.as=1) {
	if (length(a) != 2 || length(b) != 2 || length(c) != 2) {
		stop("a, b and c must be lon-lat vectors of length 2!")
	}
	if (abs(ccw.defined.as) != 1) {
		stop("clockwise.defined.as must be one of -1 and 1!")
	}
	a = sl.lonlat2xyz(a)
	b = sl.lonlat2xyz(b)
	c = sl.lonlat2xyz(c)
	alpha = b - a
	beta = c - a
	alpha.x.beta = sl.crossvec(alpha,beta)
	alpha.x.beta.dot.a = alpha.x.beta %*% a
	return(as.vector(sign(alpha.x.beta.dot.a)*ccw.defined.as))
}
