sl.line.line.intersect <-
function(line1.lon,line1.lat,line2.lon,line2.lat) {
	
	l1.cross = sl.crossvec(sl.lonlat2xyz(c(line1.lon[1],line1.lat[1])),sl.lonlat2xyz(c(line1.lon[2],line1.lat[2])))
	l2.cross = sl.crossvec(sl.lonlat2xyz(c(line2.lon[1],line2.lat[1])),sl.lonlat2xyz(c(line2.lon[2],line2.lat[2])))
	l1l2.cross = sl.crossvec(l1.cross,l2.cross)
	
	p1 = sl.xyz2lonlat(l1l2.cross)
	p2 = c(p1[1]+180,-p1[2])
	if (p2[1] > 180) {p2[1] = p2[1] - 360}
	
	p1inl1 = sl.point.between(p1[1],p1[2],line1.lon,line1.lat)
	p1inl2 = sl.point.between(p1[1],p1[2],line2.lon,line2.lat)
	p2inl1 = sl.point.between(p2[1],p2[2],line1.lon,line1.lat)
	p2inl2 = sl.point.between(p2[1],p2[2],line2.lon,line2.lat)
	
	p1inboth = (p1inl1 && p1inl2)
	p2inboth = (p2inl1 && p2inl2)
	
	if ((p2inl1 + p2inl2) > (p1inl1 + p1inl2)) {
		return(list(lines.intersect=p2inboth,lon=p2[1],lat=p2[2],lon2=p1[1],lat2=p1[2],x=-l1l2.cross[1],y=-l1l2.cross[2],z=-l1l2.cross[3],x2=l1l2.cross[1],y2=l1l2.cross[2],z2=l1l2.cross[3]))
	} else {
		return(list(lines.intersect=p1inboth,lon=p1[1],lat=p1[2],lon2=p2[1],lat2=p2[2],x=l1l2.cross[1],y=l1l2.cross[2],z=l1l2.cross[3],x2=-l1l2.cross[1],y2=-l1l2.cross[2],z2=-l1l2.cross[3]))
	}
	
}
