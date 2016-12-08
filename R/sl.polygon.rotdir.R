sl.polygon.rotdir <-
function(lon,lat) {
	
	stop("this function is not finished yet")
	
	if (length(a) != 3 || length(b) != 3) {
		stop("a and b must be vectors of length 3!")
	}
	c = numeric(3)
	c[1] = a[2]*b[3] - a[3]*b[2]
	c[2] = a[3]*b[1] - a[1]*b[3]
	c[3] = a[1]*b[2] - a[2]*b[1]
	return(c)
}
