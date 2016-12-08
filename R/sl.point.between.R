sl.point.between <-
function(p.lon,p.lat,line.lon,line.lat) {
	
	# given that the point is located on the great circle line, the function evaluates whether
	# the point lies on the shorther of the two connections (or equals one of the points)
	
	alpha = p.lon + 90
	beta = 90 - p.lat
	gamma = 0
	l.rot = sl.rot(line.lon,line.lat,alpha,beta,gamma)
	if (l.rot$lat[1] == 90 || l.rot$lat[2] == 90) {
		res = TRUE
	} else if (abs(l.rot$lon[1] - l.rot$lon[2]) < 90) {
		res = FALSE
	} else if (sum(l.rot$lat) < 0) {
		res = FALSE
	} else {
		res = TRUE
	}
	
	return(res)
	
}
