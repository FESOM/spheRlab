sl.line.lat.intersect <-
function(line.lon,line.lat,lat0) {
	
	p.cross.xyz = sl.crossvec(sl.lonlat2xyz(c(line.lon[1],line.lat[1])),sl.lonlat2xyz(c(line.lon[2],line.lat[2])))
	p.cross.xyz = p.cross.xyz / sl.cart.dist(p.cross.xyz)
	nx = p.cross.xyz[1]
	ny = p.cross.xyz[2]
	nz = p.cross.xyz[3]
	z0 = sin(pi*lat0/180)
	
	if (nx == 0 && ny == 0) {
		if (lat0 == 0) {
			return(list(line.lat.intersect=TRUE,lat=0,lon=NA,line.lat.intersect.twice=NA,lon2=NA,gcline.lat.intersect=Inf,x=NA,y=NA,z=0,x2=NA,y2=NA,z2=0))
		} else {
			return(list(line.lat.intersect=FALSE,lat=NA,lon=NA,line.lat.intersect.twice=FALSE,lon2=NA,gcline.lat.intersect=0,x=NA,y=NA,z=NA,x2=NA,y2=NA,z2=NA))
		}
	}
	
	p = 2 * nx * nz * z0 / (nx^2 + ny^2)
	q = (z0^2 * (ny^2 + nz^2) - ny^2 ) / (nx^2 + ny^2)
	
	inroot = (p/2)^2 - q
	if (inroot > 0) {
		x1 = -p/2 + sqrt(inroot)
		x2 = -p/2 - sqrt(inroot)
		y1 = -(nx*x1 + nz*z0) / ny
		y2 = -(nx*x2 + nz*z0) / ny
		lonlat1 = sl.xyz2lonlat(c(x1,y1,z0))
		lonlat2 = sl.xyz2lonlat(c(x2,y2,z0))
		line.lat.intersect.1 = sl.point.between(lonlat1[1],lat0,line.lon,line.lat)
		line.lat.intersect.2 = sl.point.between(lonlat2[1],lat0,line.lon,line.lat)
		if (line.lat.intersect.2 > line.lat.intersect.1) {
			return(list(line.lat.intersect=line.lat.intersect.2,lat=lat0,lon=lonlat2[1],line.lat.intersect.twice=line.lat.intersect.1,lon2=lonlat1[1],gcline.lat.intersect=2,x=x2,y=y2,z=z0,x2=x1,y2=y1,z2=z0))
		} else {
			return(list(line.lat.intersect=line.lat.intersect.1,lat=lat0,lon=lonlat1[1],line.lat.intersect.twice=line.lat.intersect.2,lon2=lonlat2[1],gcline.lat.intersect=2,x=x1,y=y1,z=z0,x2=x2,y2=y2,z2=z0))
		}
	} else {
		if (inroot == 0) {
			x1 = -p/2
			y1 = -(nx*x1 + nz*z0) / ny
			lonlat1 = sl.xyz2lonlat(c(x1,y1,z0))
			line.lat.intersect = sl.point.between(lonlat1[1],lat0,line.lon,line.lat)
			return(list(line.lat.intersect=line.lat.intersect,lat=lat0,lon=lonlat1[1],line.lat.intersect.twice=FALSE,lon2=NA,gcline.lat.intersect=1,x=x1,y=y1,z=z0,x2=NA,y2=NA,z2=NA))
		} else {
			return(list(line.lat.intersect=FALSE,lat=NA,lon=NA,line.lat.intersect.twice=FALSE,lon2=NA,gcline.lat.intersect=0,x=NA,y=NA,z=NA,x2=NA,y2=NA,z2=NA))
		}
	}
	
}
