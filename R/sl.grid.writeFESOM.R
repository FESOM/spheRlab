sl.grid.writeFESOM <-
function (outpath,grd,griddir.name="FESOM_grid",which.files=NULL,overwrite=FALSE,width=NULL,nsmall=NULL,verbose=TRUE) {
	
  pth = file.path(outpath,griddir.name)
  if (!dir.exists(pth)) {
    dir.create(pth)
  }
  if (verbose) {print(paste0("Writing grid files to ",pth))}
  
  all.files = c("nod2d","elem2d","nod3d","elem3d","aux3d")
	if (is.null(which.files)) {
	  which.files = all.files
	}
  
  for (fl in which.files) {
    
    if (sum(fl==all.files) < 1) {
      print(paste0("Skipping invalid element '",fl,"' in argument '",which.files,"'."))
      next()
    }
    flx = file.path(pth,paste0(fl,".out"))
    if (file.exists(flx)) {
      if (overwrite) {
        if (verbose) {print(paste0("Overwriting file ",flx))}
      } else {
        print(paste0("Skipping file ",flx," because it already exists. You may change the path, delete the file, or set 'overwrite=TRUE'."))
        next()
      }
    } else {
      if (verbose) {print(paste0("Writing file ",flx))}
    }
    
    if (fl == "nod2d") {
      if (is.null(nsmall)) {nsmll=4} else {nsmll=nsmall}
      if (is.null(width)) {wdth=8} else {wdth=width}
      write(format(grd$N,width=wdth),file=flx,append=FALSE)
      write.mat = matrix(nrow=grd$N,ncol=4)
      write.mat[,1] = format(1:grd$N,width=wdth)
      lon = grd$lon
      lon[lon<0] = lon[lon<0] + 360
      write.mat[,2] = format(lon,width=wdth,nsmall=nsmll)
      write.mat[,3] = format(grd$lat,width=wdth,nsmall=nsmll)
      write.mat[,4] = format(as.integer(grd$coast),width=wdth)
      write(t(write.mat),file=flx,append=TRUE,ncolumns=4)
    } else if (fl == "nod3d") {
      if (is.null(nsmall)) {nsmll=4} else {nsmll=nsmall}
      if (is.null(width)) {wdth=8} else {wdth=width}
      write(format(grd$N3D,width=wdth),file=flx,append=FALSE)
      Nabove = 0
      for (lev in 1:grd$Nlev) {
        wet = (lev <= grd$depth.lev)
        Nwet = sum(wet)
        write.mat = matrix(nrow=Nwet,ncol=5)
        if (is.null(width)) {wdth=8} else {wdth=width}
        write.mat[,1] = format((1:Nwet)+Nabove,width=wdth)
        lon = grd$lon[wet]
        lon[lon<0] = lon[lon<0] + 360
        write.mat[,2] = format(lon,width=wdth,nsmall=nsmll)
        write.mat[,3] = format(grd$lat[wet],width=wdth,nsmall=nsmll)
        if (is.null(width)) {wdth=11} else {wdth=width}
        write.mat[,4] = format(rep(grd$depth[lev]*(-1),Nwet),width=wdth,nsmall=nsmll)
        if (is.null(width)) {wdth=3} else {wdth=width}
        write.mat[,5] = format(grd$boundary[(1:Nwet)+Nabove],width=wdth)
        write(t(write.mat),file=flx,append=TRUE,ncolumns=5)
        Nabove = Nabove + Nwet
      }
    } else {print(paste0("Ups, ",fl," not yet implemented."))}
    
  }
  
	return(NULL)
	
}