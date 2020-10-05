sl.grid.writeCDO <-
function (grid,ofile="~/sl.grid.CDO.nc",netcdf=TRUE,netcdf.prec="double",ascii.digits=Inf,overwrite=FALSE,verbose=TRUE,cell_area=TRUE,node_node_links=TRUE,triag_nodes=TRUE,coast=TRUE,depth=TRUE,ofile.ZAXIS=paste0(ofile,"_zaxis.txt"),fesom2velocities=FALSE,conventions="original") {
	
  fun.call = deparse(sys.call(),width.cutoff=500)
  
	if (netcdf) {require("ncdf4")}
  if (cell_area && is.null(grid$cellareas)) {
    warning("'grid' does not contain an element 'cell_area'; setting 'cell_area' FALSE")
    cell_area = FALSE
  }
  if (node_node_links && is.null(grid$neighnodes)) {
    warning("'grid' does not contain an element 'neighnodes'; setting 'node_node_links' FALSE")
    node_node_links = FALSE
  }
  if (triag_nodes && is.null(grid$elem)) {
    warning("'grid' does not contain an element 'elem'; setting 'triag_nodes' FALSE")
    triag_nodes = FALSE
  }
  if (coast) {
    if (fesom2velocities) {
      if (is.null(grid$elemcoast)) {
        warning("'grid' does not contain an element 'elemcoast'; setting 'coast' FALSE")
        coast = FALSE
      }
    } else {
      if (is.null(grid$coast)) {
        warning("'grid' does not contain an element 'coast'; setting 'coast' FALSE")
        coast = FALSE
      }
    }
  }
  if (depth) {
    if (is.null(grid$Nlev)) {
      warning("'grid' does not contain an element 'Nlev'; setting 'depth' FALSE")
      depth = FALSE
    } else if (is.null(grid$depth.bounds)) {
      warning("'grid' does not contain an element 'depth.bounds'; setting 'depth' FALSE")
      depth = FALSE
    } else if (!fesom2velocities && is.null(grid$depth.lev)) {
      warning("'grid' does not contain an element 'depth.lev'; setting 'depth' FALSE")
      depth = FALSE
    } else if (fesom2velocities && is.null(grid$elemdepth.lev)) {
      warning("'grid' does not contain an element 'elemdepth.lev'; setting 'depth' FALSE")
      depth = FALSE
    }
  }
  
	N = length(grid$lon)
	if (!fesom2velocities) {
	  if (is.null(grid$stamppoly.lon)) {grid$stamppoly.lon = grid$lon_bounds}
	  if (is.null(grid$stamppoly.lat)) {grid$stamppoly.lat = grid$lat_bounds}
	  maxNstamp = ncol(grid$stamppoly.lon)
	  if (verbose) {print(paste0("the grid has ",N," nodes (grid points) with up to ",maxNstamp," stamp polygon vertices per node."))}
	} else {
	  M = nrow(grid$elem)
	  if (verbose) {
	    print(paste0("writing grid description for values defined at the centroids of the triangular elements instead of at the vertices."))
	    print(paste0("the grid has ",M," triangular elements."))
	  }
	}
	if (depth) {
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
	  
	  if (conventions == "IFS") {
	    if (!fesom2velocities) {
	      ncells.dim.name = "grid_size"
	      vertices.dim.name = "grid_corners"
	      ntriags.dim.name = "ntriags"
	      Three.dim.name = "Three"
	    } else {
	      ncells.dim.name = "ncells"
	      vertices.dim.name = "vertices"
	      ntriags.dim.name = "grid_size"
	      Three.dim.name = "grid_corners"
	    }
	    lon.var.name = "grid_center_lon"
	    lon_bnds.var.name = "grid_corner_lon"
	    lon.units.name = "degrees"
	    lat.var.name = "grid_center_lat"
	    lat_bnds.var.name = "grid_corner_lat"
	    lat.units.name = "degrees"
	  }
	  else {
	    ncells.dim.name = "ncells"
	    vertices.dim.name = "vertices"
	    ntriags.dim.name = "ntriags"
	    Three.dim.name = "Three"
	    lon.var.name = "lon"
	    lon_bnds.var.name = "lon_bnds"
	    lon.units.name = "degrees_east"
	    lat.var.name = "lat"
	    lat_bnds.var.name = "lat_bnds"
	    lat.units.name = "degrees_north"
	  }
	  
	  ncells.dim = ncdim_def(name=ncells.dim.name,units="",vals=1:N,create_dimvar=FALSE)
	  if (!fesom2velocities) {
	    vertices.dim = ncdim_def(name=vertices.dim.name,units="",vals=1:maxNstamp,create_dimvar=FALSE)
	    lon.var = ncvar_def(name=lon.var.name,units=lon.units.name,dim=ncells.dim,prec=netcdf.prec)
	    lon_bnds.var = ncvar_def(name=lon_bnds.var.name,units=lon.units.name,dim=list(vertices.dim,ncells.dim),prec=netcdf.prec)
	    lat.var = ncvar_def(name=lat.var.name,units=lat.units.name,dim=ncells.dim,prec=netcdf.prec)
	    lat_bnds.var = ncvar_def(name=lat_bnds.var.name,units=lat.units.name,dim=list(vertices.dim,ncells.dim),prec=netcdf.prec)
	  } else {
	    ntriags.dim = ncdim_def(name=ntriags.dim.name,units="",vals=1:M,create_dimvar=FALSE)
	    Three.dim = ncdim_def(name=Three.dim.name,units="",vals=1:3,create_dimvar=FALSE)
	    lon.var = ncvar_def(name=lon.var.name,units=lon.units.name,dim=ntriags.dim,prec=netcdf.prec)
	    lon_bnds.var = ncvar_def(name=lon_bnds.var.name,units=lon.units.name,dim=list(Three.dim,ntriags.dim),prec=netcdf.prec)
	    lat.var = ncvar_def(name=lat.var.name,units=lat.units.name,dim=ntriags.dim,prec=netcdf.prec)
	    lat_bnds.var = ncvar_def(name=lat_bnds.var.name,units=lat.units.name,dim=list(Three.dim,ntriags.dim),prec=netcdf.prec)
	  }

	  var.list = list(lon.var,lon_bnds.var,lat.var,lat_bnds.var)
	  
	  if (conventions == "IFS") {
	    grid_dims.dim = ncdim_def(name="grid_rank",units="",vals=1:1,create_dimvar=FALSE)
	    grid_dims.var = ncvar_def(name="grid_dims",units="",dim=grid_dims.dim,missval=-1,longname="grid dimensions",prec="integer")
	    var.list[[length(var.list)+1]] = grid_dims.var
	    if (!fesom2velocities) {
	      grid_imask.var = ncvar_def(name="grid_imask",units="unitless",dim=ncells.dim,missval=-1,longname="land sea mask",prec="integer")
	    } else {
	      grid_imask.var = ncvar_def(name="grid_imask",units="unitless",dim=ntriags.dim,missval=-1,longname="land sea mask",prec="integer")
	    }
	    var.list[[length(var.list)+1]] = grid_imask.var
	  }
	  
	  if (cell_area) {
	    if (!fesom2velocities) {
	      cellareas.var = ncvar_def(name="cell_area",units="m2",dim=ncells.dim,missval=-1,longname="area of grid cell",prec=netcdf.prec)
	    } else {
	      cellareas.var = ncvar_def(name="cell_area",units="m2",dim=ntriags.dim,missval=-1,longname="area of grid cell",prec=netcdf.prec)
	    }
	    var.list[[length(var.list)+1]] = cellareas.var
	  }
	  
	  if (node_node_links) {
	    nlinks_max.dim = ncdim_def(name="nlinks_max",units="",vals=1:ncol(grid$neighnodes),create_dimvar=FALSE)
	    node_node_links.var = ncvar_def(name="node_node_links",units="",dim=list(nlinks_max.dim,ncells.dim),missval=-1,longname="Indicates which other nodes neighbour each node.",prec="integer")
	    var.list[[length(var.list)+1]] = node_node_links.var
	  }
	  
	  if (triag_nodes) {
	    if (!fesom2velocities) {
	      ntriags.dim = ncdim_def(name=ntriags.dim.name,units="",vals=1:nrow(grid$elem),create_dimvar=FALSE)
	      Three.dim = ncdim_def(name=Three.dim.name,units="",vals=1:3,create_dimvar=FALSE)
	    }
	    triag_nodes.var = ncvar_def(name="triag_nodes",units="",dim=list(Three.dim,ntriags.dim),missval=-1,longname="Maps every triangular face to its three corner nodes.",prec="integer")
	    var.list[[length(var.list)+1]] = triag_nodes.var
	  }
	  
	  if (coast) {
	    if (!fesom2velocities) {
	      coast.var = ncvar_def(name="coast",units="",dim=ncells.dim,missval=-1,longname="Indicates coastal nodes: coast=1, internal=0",prec="integer")
	    } else {
	      coast.var = ncvar_def(name="coast",units="",dim=ntriags.dim,missval=-1,longname="Indicates coastal triangles: coast=1, internal=0",prec="integer")
	    }
	    var.list[[length(var.list)+1]] = coast.var
	  }
	  
	  if (depth) {
	    nlev.dim = ncdim_def(name="nlev",units="",vals=1:Nlev,create_dimvar=FALSE)
	    depth.var = ncvar_def(name="depth",units="m",dim=nlev.dim,missval=-1,longname="depth of model levels in metres (positive downwards)",prec=netcdf.prec)
	    nlev_bnds.dim = ncdim_def(name="nlev_bnds",units="",vals=1:(Nlev+1),create_dimvar=FALSE)
	    depth_bnds.var = ncvar_def(name="depth_bnds",units="m",dim=nlev_bnds.dim,missval=-1,longname="depth of model level bounds in metres (positive downwards)",prec=netcdf.prec)
	    if (!fesom2velocities) {
	      depth_lev.var = ncvar_def(name="depth_lev",units="",dim=ncells.dim,missval=-1,longname="depth in terms of number of active levels beneath each ocean surface grid point",prec="integer")
	    } else {
	      depth_lev.var = ncvar_def(name="depth_lev",units="",dim=ntriags.dim,missval=-1,longname="depth in terms of number of active levels beneath each ocean surface element",prec="integer")
	    }
	    var.list[[length(var.list)+1]] = depth.var
	    var.list[[length(var.list)+1]] = depth_bnds.var
	    var.list[[length(var.list)+1]] = depth_lev.var
	  }
	  
	  ofl = nc_create(filename = ofile, vars = var.list, force_v4 = TRUE)
	  
	  if (!fesom2velocities) {
	    if (conventions == "IFS") {
	      grid$lon[grid$lon < 0] = grid$lon[grid$lon < 0] + 360
	      grid$stamppoly.lon[grid$stamppoly.lon < 0] = grid$stamppoly.lon[grid$stamppoly.lon < 0] + 360
	    }
	    ncvar_put(ofl,lon.var.name,vals=grid$lon)
	    ncvar_put(ofl,lon_bnds.var.name,vals=t(grid$stamppoly.lon))
	    ncvar_put(ofl,lat.var.name,vals=grid$lat)
	    ncvar_put(ofl,lat_bnds.var.name,vals=t(grid$stamppoly.lat))
	  } else {
	    if (conventions == "IFS") {
	      grid$lon[grid$lon < 0] = grid$lon[grid$lon < 0] + 360
	      grid$baryc.lon[grid$baryc.lon < 0] = grid$baryc.lon[grid$baryc.lon < 0] + 360
	    }
	    ncvar_put(ofl,lon.var.name,vals=grid$baryc.lon)
	    vals = t(grid$elem) * NA
	    vals[] = grid$lon[t(grid$elem)]
	    ncvar_put(ofl,lon_bnds.var.name,vals=vals)
	    ncvar_put(ofl,lat.var.name,vals=grid$baryc.lat)
	    vals[] = grid$lat[t(grid$elem)]
	    ncvar_put(ofl,lat_bnds.var.name,vals=vals)
	    rm(vals)
	  }
	  
	  if (conventions == "IFS") {
	    if (!fesom2velocities) {
	      ncvar_put(ofl,"grid_dims",vals=N)
	      ncvar_put(ofl,"grid_imask",vals=rep(1,N))
	    } else {
	      ncvar_put(ofl,"grid_dims",vals=M)
	      ncvar_put(ofl,"grid_imask",vals=rep(1,M))
	    }
	  }
	  
	  ncatt_put(ofl,lon.var.name,"standard_name","longitude")
	  ncatt_put(ofl,lon.var.name,"bounds",lon_bnds.var.name)
	  
	  ncatt_put(ofl,lon_bnds.var.name,"standard_name","longitude_bounds")
	  ncatt_put(ofl,lon_bnds.var.name,"centers",lon.var.name)
	  
	  ncatt_put(ofl,lat.var.name,"standard_name","latitude")
	  ncatt_put(ofl,lat.var.name,"bounds",lat_bnds.var.name)
	  
	  ncatt_put(ofl,lat_bnds.var.name,"standard_name","latitude_bounds")
	  ncatt_put(ofl,lat_bnds.var.name,"centers",lat.var.name)
	  
	  if (cell_area) {
	    if (!fesom2velocities) {
	      ncvar_put(ofl,"cell_area",vals=grid$cellareas)
	    } else {
	      ncvar_put(ofl,"cell_area",vals=grid$elemareas)
	    }
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
	    if (!fesom2velocities) {
	      ncvar_put(ofl,"coast",vals=grid$coast)
	    } else {
	      ncvar_put(ofl,"coast",vals=grid$elemcoast)
	    }
	    ncatt_put(ofl,"coast","grid_type","unstructured")
	    ncatt_put(ofl,"coast","coordinates","lat lon")
	  }
	  if (depth) {
	    ncvar_put(ofl,"depth",vals=grid$depth)
	    ncvar_put(ofl,"depth_bnds",vals=grid$depth.bounds)
	    if (!fesom2velocities) {
	      ncvar_put(ofl,"depth_lev",vals=grid$depth.lev)
	    } else {
	      ncvar_put(ofl,"depth_lev",vals=grid$elemdepth.lev)
	    }
	    ncatt_put(ofl,"depth_lev","grid_type","unstructured")
	    ncatt_put(ofl,"depth_lev","coordinates","lat lon")
	  }
	  
	  ncatt_put(ofl,0,"Conventions","CF-1.4")
	  history.att = paste0(strftime(Sys.time(),tz = "GMT",usetz=T),"; Grid description file generated with spheRlab version ",packageVersion("spheRlab"))
	  if (!is.null(grid$fun.call)) {
	    history.att = paste0(history.att,"; Grid read and converted with: ",grid$fun.call)
	  }
	  history.att = paste0(history.att,"; Grid written with: ",fun.call)
	  ncatt_put(ofl,0,"history",history.att)
	  
	  nc_close(ofl)
	  
	} else {
	  
	  if (fesom2velocities) {
	    stop("ascii output (which is deprecated and slow anyway) has not been implemented for fesom2velocities=TRUE.")
	  }
	  
	  warning("Be aware that you are writing to ascii (instead of NetCDF) which is much slower.")
	  
	  write("gridtype  = unstructured",ofile)
	  write(paste("gridsize  =",N,sep=" "),ofile,append=TRUE)
	  write(paste("nvertex   =",maxNstamp,sep=" "),ofile,append=TRUE)
	  if (verbose) {print("writing node longitudes ...")}
	  write(paste("xvals   =",round(grid$lon[1],ascii.digits)),ofile,append=TRUE)
	  for (i in 2:N) {
	    write(paste("         ",round(grid$lon[i],ascii.digits),sep=" "),ofile,append=TRUE)
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
	      strvec = paste(strvec,round(grid$stamppoly.lon[i,j],ascii.digits),sep=" ")
	    }
	    write(strvec,ofile,append=TRUE)
	  }
	  if (verbose) {print("...done.")}
	  if (verbose) {print("writing node latitudes ...")}
	  write(paste("yvals   =",round(grid$lat[1],ascii.digits)),ofile,append=TRUE)
	  for (i in 2:N) {
	    write(paste("         ",round(grid$lat[i],ascii.digits),sep=" "),ofile,append=TRUE)
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
	      strvec = paste(strvec,round(grid$stamppoly.lat[i,j],ascii.digits),sep=" ")
	    }
	    write(strvec,ofile,append=TRUE)
	  }
    
	}
	
	if (verbose) {print("horizontal grid description file complete.")}
	if (verbose) {print(paste0("you can use this file to set the horizontal grid of a corresponding NetCDF file with 'cdo setgrid,",ofile," ifile.nc ofile.nc'."))}
	
	if (depth && !is.null(ofile.ZAXIS)) {
	  res = sl.grid.writeZAXIS(grid,ofile=ofile.ZAXIS,overwrite=overwrite,verbose=verbose)
	}
	
}
