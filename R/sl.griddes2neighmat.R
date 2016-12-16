sl.griddes2neighmat <-
function (CDOgriddes,critnum.neigh=2,critnum.coast=1) {
	
	require(ncdf4)
	
	griddes.fl = nc_open(CDOgriddes)
	lat = ncvar_get(griddes.fl,varid="lat")
	lon = ncvar_get(griddes.fl,varid="lon")
	lat_vertices = ncvar_get(griddes.fl,varid="lat_vertices")
	lon_vertices = ncvar_get(griddes.fl,varid="lon_vertices")
	nc_close(griddes.fl)
	
	N = length(lat)
	Nv = dim(lat_vertices)[2]
	maxmaxneigh = dim(lat_vertices)[1]
	neighmat = matrix(nrow=N,ncol=maxmaxneigh)
	is.coast = rep(FALSE,N)
	
	for (n in 1:N) {
		
		m = 1
		vert_ident.N = rep(0,N)
		neigh = 1
		
		while (m <= maxmaxneigh) {
			lat_vert1 = lat_vertices[m,n]
			lon_vert1 = lon_vertices[m,n]
			compare.mat = (lat_vertices == lat_vert1) * (lon_vertices == lon_vert1)
			if ((sum(compare.mat)-sum(compare.mat[,n])) < critnum.coast) {
				is.coast[n] = TRUE
			}
			vert_ident.N.prev = vert_ident.N
			vert_ident.N[colSums(compare.mat,na.rm=TRUE) > 0] = vert_ident.N[colSums(compare.mat,na.rm=TRUE) > 0] + 1
			add.neigh = ((vert_ident.N.prev == critnum.neigh-1) & (vert_ident.N == critnum.neigh))
			add.neigh[n] = FALSE
			if (sum(add.neigh) > 0) {
				#neighmat[n,neigh] = (1:N)[add.neigh]
				neighmat[n,neigh] = which(add.neigh)
				neigh = neigh + 1
			}
			if (m == maxmaxneigh) {break}
			if (lat_vertices[m+1,n] == lat_vert1 && lon_vertices[m+1,n] == lon_vert1) {break}
			m = m + 1
		}
		
	}
	
	Nneigh = rowSums(!is.na(neighmat))
	
	return(list(lat=lat,lon=lon,neighmat=neighmat[,1:max(Nneigh)],Nneigh=Nneigh,is.coast=is.coast,critnum.neigh=critnum.neigh,critnum.coast=critnum.coast))
	
}
