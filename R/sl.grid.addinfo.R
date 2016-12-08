sl.grid.addinfo <-
function (grid,ncdf.file.in,ncdf.file.out=paste0(ncdf.file.in,"_addinfo.nc"),add.cell_area=TRUE,add.node_node_links=TRUE,add.triag_nodes=TRUE,add.coast=TRUE) {
	
	if (!any(add.cell_area,add.node_node_links,add.triag_nodes,add.coast)) {
		stop("no variables selected to be added")
	}
		
	require("ncdf4")
	
	if (ncdf.file.out != ncdf.file.in) {
		system.res = system(paste("cp",ncdf.file.in,ncdf.file.out,sep=" "))
	}
	
	iofl = nc_open(ncdf.file.out,write=TRUE)
	
	if (add.cell_area) {
		ncells.dim = iofl$dim$ncells
		cellareas.var = ncvar_def(name="cell_area",units="m2",dim=ncells.dim,missval=-1,longname="area of grid cell")
		ncvar_add(iofl,cellareas.var)
	}
	
	if (add.node_node_links) {
		ncells.dim = iofl$dim$ncells
		nlinks_max.dim = ncdim_def(name="nlinks_max",units="",vals=1:ncol(grid$neighnodes),create_dimvar=FALSE)
		node_node_links = ncvar_def(name="node_node_links",units="",dim=list(nlinks_max.dim,ncells.dim),missval=-1,longname="Indicates which other nodes neighbour each node.",prec="integer")
		ncvar_add(iofl,node_node_links)
	}
		
	if (add.triag_nodes) {
		ntriags.dim = ncdim_def(name="ntriags",units="",vals=1:nrow(grid$elem),create_dimvar=FALSE)
		Three.dim = ncdim_def(name="Three",units="",vals=1:3,create_dimvar=FALSE)
		triag_nodes = ncvar_def(name="triag_nodes",units="",dim=list(Three.dim,ntriags.dim),missval=-1,longname="Maps every triangular face to its three corner nodes.",prec="integer")
		ncvar_add(iofl,triag_nodes)
	}
	
	if (add.coast) {
		ncells.dim = iofl$dim$ncells
		coast.var = ncvar_def(name="coast",units="",dim=ncells.dim,missval=-1,longname="Indicates coastal nodes: coast=1, internal=0",prec="integer")
		ncvar_add(iofl,coast.var)
	}
	
	nc_close(iofl)
	
	iofl = nc_open(ncdf.file.out,write=TRUE)
	
	if (add.cell_area) {
		ncvar_put(iofl,"cell_area",vals=grid$cellareas)
	}
	
	if (add.node_node_links) {
		ncvar_put(iofl,"node_node_links",vals=t(grid$neighnodes))
	}
	
	if (add.triag_nodes) {
		ncvar_put(iofl,"triag_nodes",vals=t(grid$elem))
	}
	
	if (add.coast) {
		ncvar_put(iofl,"coast",vals=grid$coast)
	}
	
	nc_close(iofl)

}
