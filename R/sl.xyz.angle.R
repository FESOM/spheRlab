sl.xyz.angle <-
function (a,b) {
	
	return(acos(drop(a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))) * 360 / (2*pi))
	
	### old, inefficient implementation:
	#a.lonlat = sl.xyz2lonlat(a)
	#b.lonlat = sl.xyz2lonlat(b)
	#return(sl.lonlat.angle(a.lonlat[1],a.lonlat[2],b.lonlat[1],b.lonlat[2]))
	
}
