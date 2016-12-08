sl.elemfull2elemsub <-
function (nodes.full.lon,nodes.full.lat,nodes.sub.lon,nodes.sub.lat,elem.full,tolerance=0) {
	
	Nfull = length(nodes.full.lon)
	Nsub = length(nodes.sub.lon)
	full2sub = rep(NA,Nsub)
	sub2full = rep(NA,Nfull)
	if (tolerance == 0) {
		for (n in 1:Nsub) {
			m = which(nodes.full.lon == nodes.sub.lon[n])
			m = m[nodes.full.lat[m] == nodes.sub.lat[n]]
			if (length(m) != 1) {
				if (length(m) == 0) {stop(paste("node",n,"in griddes.sub not contained in griddes.full"))}
				stop(paste("node",n,"in griddes.sub more than once contained in griddes.full"))
			}
			full2sub[n] = m
			sub2full[m] = n
		}
	} else {
		for (n in 1:Nsub) {
			m = which(abs(nodes.full.lon - nodes.sub.lon[n]) <= tolerance)
			m = m[abs(nodes.full.lat[m] - nodes.sub.lat[n]) <= tolerance]
			if (length(m) != 1) {
				if (length(m) == 0) {stop(paste("node",n,"in griddes.sub not contained in griddes.full"))}
				stop(paste("node",n,"in griddes.sub more than once contained in griddes.full, maybe tolerance too high"))
			}
			full2sub[n] = m
			sub2full[m] = n
		}
	}
	
	Nelem.full = nrow(elem.full)
	Melem.full = ncol(elem.full)
	if (Nelem.full < Melem.full) {warning("elem.full has more columns than rows and probably needs to be transposed, e.g. using t(elem.full) as input")}
	elem.sub = elem.full * NA
	for (ne in 1:Nelem.full) {
		elem.ne = elem.full[ne,]
		elem.ne.l = min(match(NA,elem.ne)-1,Melem.full,na.rm=TRUE)
		elem.ne = elem.ne[1:elem.ne.l]
		elem.ne.sub = sub2full[elem.ne]
		if (sum(is.na(sub2full[elem.ne])) == 0) {
			elem.sub[ne,1:elem.ne.l] = sub2full[elem.ne]   # stimmt das???
		}
	}
	elem.sub = elem.sub[!is.na(elem.sub[,1]),]
	
	return(list(elem.sub=elem.sub,full2sub=full2sub,sub2full=sub2full))
	
}
