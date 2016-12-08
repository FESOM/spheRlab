sl.grid.writeCDO <-
function (grid,ofile="~/sl.grid.CDO",netcdf=FALSE,write.precision=Inf,overwrite=FALSE,verbose=TRUE) {
	
	if (netcdf) {
		stop("direct netcdf output not yet implemented. Set 'netcdf=FALSE' to generate ascii output that can be coverted to netcdf with a single-line CDO command (see example in function documentation).")
		require("ncdf4")
	}
	
	N = length(grid$lon)
	maxNstamp = ncol(grid$stamppoly.lon)
	if (verbose) {print(paste0("the grid has ",N," nodes (grid points) with up to ",maxNstamp," stamp polygon vertices per node."))}
	
	if (file.exists(ofile)) {
		if (overwrite) {
			warning("overwriting existing file ...")
		} else {
			stop(paste0("file",ofile," already exists. rename/delete original file or set 'overwrite=TRUE'."))
		}
	}
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
