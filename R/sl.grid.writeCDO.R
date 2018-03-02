sl.grid.writeCDO <-
function (grid,ofile="~/sl.grid.CDO.nc",netcdf=TRUE,netcdf.prec="double",ascii.digits=Inf,overwrite=FALSE,verbose=TRUE,cell_area=TRUE,node_node_links=TRUE,triag_nodes=TRUE,coast=TRUE,depth=TRUE) {
	
	if (netcdf) {require("ncdf4")}
	
	N = length(grid$lon)
	maxNstamp = ncol(grid$stamppoly.lon)
	if (verbose) {print(paste0("the grid has ",N," nodes (grid points) with up to ",maxNstamp," stamp polygon vertices per node."))}
	if (depth) {
	  if (is.null(grid$Nlev)) {stop("grid apparently has no depth information (at least no element Nlev). Rerun with depth=FALSE or add 3D variables to the grid.")}
	  Nlev = grid$Nlev
	  if (verbose) {print(paste0("the grid has ",Nlev," vertical levels."))}
	}
	
	if (file.exists(ofile)) {
		if (overwrite) {
			warning("overwriting existing file ...")
		} else {
			stop(paste0("file ",ofile," already exists. rename/delete original file or set 'overwrite=TRUE'."))
		}
	}
	
	if (netcdf) {
	  
	  ncells.dim = ncdim_def(name="ncells",units="",vals=1:N,create_dimvar=FALSE)
	  vertices.dim = ncdim_def(name="vertices",units="",vals=1:maxNstamp,create_dimvar=FALSE)
	  
	  lon.var = ncvar_def(name="lon",units="degrees_east",dim=ncells.dim,prec=netcdf.prec)
	  lon_bnds.var = ncvar_def(name="lon_bnds",units="degrees_east",dim=list(vertices.dim,ncells.dim),prec=netcdf.prec)
	  lat.var = ncvar_def(name="lat",units="degrees_north",dim=ncells.dim,prec=netcdf.prec)
	  lat_bnds.var = ncvar_def(name="lat_bnds",units="degrees_north",dim=list(vertices.dim,ncells.dim),prec=netcdf.prec)

	  var.list = list(lon.var,lon_bnds.var,lat.var,lat_bnds.var)
	  
	  if (cell_area) {
	    cellareas.var = ncvar_def(name="cell_area",units="m2",dim=ncells.dim,missval=-1,longname="area of grid cell",prec=netcdf.prec)
	    var.list[[length(var.list)+1]] = cellareas.var
	  }
	  if (node_node_links) {
	    nlinks_max.dim = ncdim_def(name="nlinks_max",units="",vals=1:ncol(grid$neighnodes),create_dimvar=FALSE)
	    node_node_links.var = ncvar_def(name="node_node_links",units="",dim=list(nlinks_max.dim,ncells.dim),missval=-1,longname="Indicates which other nodes neighbour each node.",prec="integer")
	    var.list[[length(var.list)+1]] = node_node_links.var
	  }
	  if (triag_nodes) {
	    ntriags.dim = ncdim_def(name="ntriags",units="",vals=1:nrow(grid$elem),create_dimvar=FALSE)
	    Three.dim = ncdim_def(name="Three",units="",vals=1:3,create_dimvar=FALSE)
	    triag_nodes.var = ncvar_def(name="triag_nodes",units="",dim=list(Three.dim,ntriags.dim),missval=-1,longname="Maps every triangular face to its three corner nodes.",prec="integer")
	    var.list[[length(var.list)+1]] = triag_nodes.var
	  }
	  if (coast) {
	    coast.var = ncvar_def(name="coast",units="",dim=ncells.dim,missval=-1,longname="Indicates coastal nodes: coast=1, internal=0",prec="integer")
	    var.list[[length(var.list)+1]] = coast.var
	  }
	  if (depth) {
	    nlev.dim = ncdim_def(name="nlev",units="",vals=1:Nlev,create_dimvar=FALSE)
	    depth.var = ncvar_def(name="depth",units="m",dim=nlev.dim,missval=-1,longname="depth of model levels in metres (positive downwards)",prec=netcdf.prec)
	    depth_lev.var = ncvar_def(name="depth_lev",units="",dim=ncells.dim,missval=-1,longname="depth in terms of number of active levels beneath each ocean surface grid point",prec="integer")
	    var.list[[length(var.list)+1]] = depth.var
	    var.list[[length(var.list)+1]] = depth_lev.var
	  }
	  
	  ofl = nc_create(filename = ofile, vars = var.list)
	  
	  ncvar_put(ofl,"lon",vals=grid$lon)
	  ncatt_put(ofl,"lon","standard_name","longitude")
	  ncatt_put(ofl,"lon","bounds","lon_bnds")
	  
	  ncvar_put(ofl,"lon_bnds",vals=t(grid$stamppoly.lon))
	  ncatt_put(ofl,"lon_bnds","standard_name","longitude_bounds")
	  ncatt_put(ofl,"lon_bnds","centers","lon")
	  
	  ncvar_put(ofl,"lat",vals=grid$lat)
	  ncatt_put(ofl,"lat","standard_name","latitude")
	  ncatt_put(ofl,"lat","bounds","lat_bnds")
	  
	  ncvar_put(ofl,"lat_bnds",vals=t(grid$stamppoly.lat))
	  ncatt_put(ofl,"lat_bnds","standard_name","latitude_bounds")
	  ncatt_put(ofl,"lat_bnds","centers","lat")
	  
	  if (cell_area) {
	    ncvar_put(ofl,"cell_area",vals=grid$cellareas)
	    ncatt_put(ofl,"cell_area","grid_type","unstructured")
	    ncatt_put(ofl,"cell_area","coordinates","lat lon")
	  }
	  if (node_node_links) {
	    ncvar_put(ofl,"node_node_links",vals=t(grid$neighnodes))
	  }
	  if (triag_nodes) {
	    ncvar_put(ofl,"triag_nodes",vals=t(grid$elem))
	  }
	  if (coast) {
	    ncvar_put(ofl,"coast",vals=grid$coast)
	    ncatt_put(ofl,"coast","grid_type","unstructured")
	    ncatt_put(ofl,"coast","coordinates","lat lon")
	  }
	  if (depth) {
	    ncvar_put(ofl,"depth",vals=grid$depth)
	    ncvar_put(ofl,"depth_lev",vals=grid$depth.lev)
	    ncatt_put(ofl,"depth_lev","grid_type","unstructured")
	    ncatt_put(ofl,"depth_lev","coordinates","lat lon")
	  }
	  
	  ncatt_put(ofl,0,"Conventions","CF-1.4")
	  ncatt_put(ofl,0,"history","Grid description file generated with spheRlab sl.grid.writeCDO")
	  
	  nc_close(ofl)
	  
	} else {
	  
	  warning("Be aware that you are writing to ascii (instead of NetCDF) which is much slower.")
	  
	  write("gridtype  = unstructured",ofile)
	  write(paste("gridsize  =",N,sep=" "),ofile,append=TRUE)
	  write(paste("nvertex   =",maxNstamp,sep=" "),ofile,append=TRUE)
	  if (verbose) {print("writing node longitudes ...")}
	  write(paste("xvals   =",round(grid$lon[1],write.precision)),ofile,append=TRUE)
	  for (i in 2:N) {
	    write(paste("         ",round(grid$lon[i],write.precision),sep=" "),ofile,append=TRUE)
	  }
	  if (verbose) {print("...done.")}
	  if (verbose) {print("writing stamp polygon vertex longitudes ...")}
	  for (i in 1:N) {
	    if (i == 1) {
	      strvec = "xbounds ="
	    } else {
	      strvec = "         "
	    }
	    for (j in 1:maxNstamp) {
	      strvec = paste(strvec,round(grid$stamppoly.lon[i,j],write.precision),sep=" ")
	    }
	    write(strvec,ofile,append=TRUE)
	  }
	  if (verbose) {print("...done.")}
	  if (verbose) {print("writing node latitudes ...")}
	  write(paste("yvals   =",round(grid$lat[1],write.precision)),ofile,append=TRUE)
	  for (i in 2:N) {
	    write(paste("         ",round(grid$lat[i],write.precision),sep=" "),ofile,append=TRUE)
	  }
	  if (verbose) {print("...done.")}
	  if (verbose) {print("writing stamp polygon vertex latitudes ...")}
	  for (i in 1:N) {
	    if (i == 1) {
	      strvec = "ybounds ="
	    } else {
	      strvec = "         "
	    }
	    for (j in 1:maxNstamp) {
	      strvec = paste(strvec,round(grid$stamppoly.lat[i,j],write.precision),sep=" ")
	    }
	    write(strvec,ofile,append=TRUE)
	  }
	  if (verbose) {print("...done. CDO grid description file complete.")}
	  if (verbose) {print(paste0("you can now convert the file to NetCDF running from the command line e.g. 'cdo -f nc const,0,",ofile," ",ofile,".nc'. Thereafter you may add grid information using 'sl.grid.addinfo'."))}
    
	}
	
}
