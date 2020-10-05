sl.grid.writeZAXIS <-
function (grid=NULL,levels=NULL,bounds=NULL,zaxistype="depth_below_sea",ofile="~/sl.grid.zaxis.txt",overwrite=FALSE,verbose=TRUE) {
  
  if (is.null(levels)) {
    if (is.null(grid) || is.null(grid$depth)) {stop("Either 'grid' (with element 'depth') or 'levels' must be specified")}
    levels = grid$depth
  } else {
    if (!is.null(grid)) {warning("ignoring 'grid' because 'levels' are specified explicitly")}
  }
  
  if (is.null(bounds)) {
    if (!is.null(grid) && !is.null(grid$depth.bounds)) {
      bounds = grid$depth.bounds
      if (length(bounds) != length(levels) + 1) {
        stop("'bounds' must contain one more element than 'levels'")
      }
    } else {
      warning("'bounds' not specified and not contained in 'grid' (element 'depth.bounds'), not adding bounds to file")
      bounds = NA
    }
  }
  
  if (file.exists(ofile)) {
    if (overwrite) {
      warning("overwriting existing file ...")
    } else {
      stop(paste0("file ",ofile," already exists. rename/delete original file or set 'overwrite=TRUE'."))
    }
  }
  
  write(paste0("zaxistype = ",zaxistype),ofile)
  write(paste0("size      = ",length(levels)),ofile,append=TRUE)
  write(paste0("levels    = ",paste(levels,collapse=" ")),ofile,append=TRUE)
  if (!is.na(bounds[1])) {
    write(paste0("lbounds   = ",paste(bounds[2:length(bounds)],collapse=" ")),ofile,append=TRUE)
    write(paste0("ubounds   = ",paste(bounds[1:(length(bounds)-1)],collapse=" ")),ofile,append=TRUE)
  }
  if (verbose) {print("z-axis description file complete.")}
  if (verbose) {print(paste0("you can use this file to set the z-axis of a corresponding NetCDF file with 'cdo setzaxis,",ofile," ifile.nc ofile.nc'."))}
  
}
