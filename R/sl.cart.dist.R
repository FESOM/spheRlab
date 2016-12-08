sl.cart.dist <-
function(a,b=NULL,norm=2) {
	
	if (!is.null(b)) {
		a = a - b
	}
	
	return((sum(a^norm))^(1/norm))
	
}
