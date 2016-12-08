sl.lonlatrot2abg <-
function(lonlatrot) {

	return(c(lonlatrot[1]+90,90-lonlatrot[2],lonlatrot[3]))
	
}
