sl.griddes2neighmat <-
function (CDOgriddes,critnum.neigh=2,critnum.coast=1) {

	#rm(list=ls())
	#CDOgriddes = "/Users/hgoessli/AWI/CDO/unstructured/mesh_ref87k_CDOgriddes_testHudson1_exact.nc"
	#critnum.neigh = 2
	
	require(ncdf)
	
	griddes.fl = open.ncdf(CDOgriddes)
	lat = get.var.ncdf(griddes.fl,varid="lat")
	lon = get.var.ncdf(griddes.fl,varid="lon")
	lat_vertices = get.var.ncdf(griddes.fl,varid="lat_vertices")
	lon_vertices = get.var.ncdf(griddes.fl,varid="lon_vertices")
	close.ncdf(griddes.fl)
	
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
