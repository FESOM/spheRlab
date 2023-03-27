sl.load.naturalearth <-
function (what="all",resolution="medium",poly.split=TRUE,naturalearth.dir=NULL,force.raw2split=FALSE,
          force.shape2raw=FALSE,force.download=FALSE,download.if.missing=TRUE,
          download.baseurl="https://www.naturalearthdata.com/http//www.naturalearthdata.com/download",read=TRUE,verbose=TRUE) {
	
	if (length(what) == 1 && (what == "all" || what == "list" )) {
		if (what == "all" && length(resolution) > 1) {stop("what='all' works only for one resolution at a time.")}
		what.orig = what
		if (resolution[1] == "coarse") {
			what = c("coastline","geographic_lines","geography_marine_polys","geography_regions_elevation_points",
			         "geography_regions_points","geography_regions_polys","glaciated_areas","graticules_1","graticules_5",
			         "graticules_10","graticules_15","graticules_20","graticules_30","lakes","land","ocean",
			         "rivers_lake_centerlines","wgs84_bounding_box")
		} else if (resolution[1] == "medium") {
			what = c("antarctic_ice_shelves_lines","antarctic_ice_shelves_polys","coastline","geographic_lines",
			         "geography_marine_polys","geography_regions_elevation_points","geography_regions_points",
			         "geography_regions_polys","glaciated_areas","graticules_1","graticules_5","graticules_10",
			         "graticules_15","graticules_20","graticules_30","lakes_historic","lakes","land","ocean","playas",
			         "rivers_lake_centerlines_scale_rank","rivers_lake_centerlines","wgs84_bounding_box")
		} else if (resolution[1] == "fine") {
			what = c("antarctic_ice_shelves_lines","antarctic_ice_shelves_polys","bathymetry_A_10000","bathymetry_B_9000",
			         "bathymetry_C_8000","bathymetry_D_7000","bathymetry_E_6000","bathymetry_F_5000","bathymetry_G_4000",
			         "bathymetry_H_3000","bathymetry_I_2000","bathymetry_J_1000","bathymetry_K_200","bathymetry_L_0",
			         "coastline","geographic_lines","geography_marine_polys","geography_regions_elevation_points",
			         "geography_regions_points","geography_regions_polys","glaciated_areas","graticules_1","graticules_5",
			         "graticules_10","graticules_15","graticules_20","graticules_30","lakes_europe","lakes_historic",
			         "lakes_north_america","lakes_pluvial","lakes","land_ocean_label_points","land_ocean_seams",
			         "land_scale_rank","land","minor_islands_coastline","minor_islands_label_points","minor_islands",
			         "ocean_scale_rank","ocean","playas","reefs","rivers_europe","rivers_lake_centerlines_scale_rank",
			         "rivers_lake_centerlines","rivers_north_america","wgs84_bounding_box")
		} else {stop("'resolution' must be one of 'coarse', 'medium', and 'fine'.")}
		if (what.orig == "list") {
			print(paste0("The following 'what' values are available for resolution='",resolution,"':"))
			print(what)
			return(NULL)
		}
	}
  
  if (force.download) {force.shape2raw = TRUE}
  if (force.shape2raw) {force.raw2split = TRUE}
	
  if (is.null(naturalearth.dir)) {
    if (file.exists("~/.spheRlab")) {
      source("~/.spheRlab",local=TRUE)
    }
    if (is.null(naturalearth.dir)) {
      naturalearth.dir = "~/naturalearthdata"
      warning(paste0("Assuming that 'naturalearth.dir' is '~/naturalearthdata'. You can specify a different default ",
                     "by setting this variable in a file '~/.spheRlab', or specify it directly with the corresponding ",
                     "function argument."))
    }
  }
  
	if (!dir.exists(naturalearth.dir)) {
		if (verbose) {print(paste0("Creating directory ",naturalearth.dir," to store Natural Earth data."))}
		dir.create(naturalearth.dir)
	}
	
	N = length(what)
	resolution = rep(resolution,ceiling(N/length(resolution)))[1:N]
	
	res.list = list()
	
	for (i in 1:N) {
		
		if (resolution[i] == "coarse") {resolution.str = "110m"}
		else if (resolution[i] == "medium") {resolution.str = "50m"}
		else if (resolution[i] == "fine") {resolution.str = "10m"}
		else {stop("'resolution' must be one of 'coarse', 'medium', and 'fine'.")}
		reswhat = paste0(resolution.str,"_",what[i])
		
		fl.split = paste0(naturalearth.dir,"/ne_",reswhat,"_split.rds")
		if (file.exists(fl.split) && !force.raw2split && poly.split) {
		  if (read) {
		    res.list[[reswhat]] = readRDS(fl.split)
		    if (!(res.list[[reswhat]]$type == "SpatialPolygons")) {stop("inconsistency, expecting type 'SpatialPolygon'")}
		  }
		  next
		}
		
		fl.r = paste0(naturalearth.dir,"/ne_",reswhat,".rds")
		if (.Platform$OS.type == "windows") {fl.r = paste(strsplit(fl.r,"/")[[1]],collapse="\\")}
		
		if (!file.exists(fl.r) || force.shape2raw) {
		  
		  fl = paste0(naturalearth.dir,"/ne_",reswhat,".shp")
		  if (.Platform$OS.type == "windows") {fl = paste(strsplit(fl,"/")[[1]],collapse="\\")}
		  
		  if (!file.exists(fl) || force.download) {
			  if (download.if.missing || force.download) {
				  if (verbose) {print(paste0("Fetching Natural Earth data for ",fl," from the internet."))}
				  download.url = paste0(download.baseurl,"/",resolution.str,"/physical/ne_",reswhat,".zip")
				  destfile = paste0(naturalearth.dir,"/ne_",reswhat,".zip")
				  if (.Platform$OS.type == "windows") {destfile = paste(strsplit(destfile,"/")[[1]],collapse="\\")}
				  download.try = try(download.file(download.url,destfile))
				  if (class(download.try) != "try-error") {
					  unzip(destfile,exdir=naturalearth.dir)
				  } else {
					  print("Download failed.")
					  next
				  }
				  unlink(destfile)
			  } else {
				  print(paste0("Natural Earth shape file ",fl," does not exist locally. If it shall be fetched from the internet, set 'download.if.missing=TRUE' or 'force.download=TRUE'."))
				  next
			  }
		  }
		  
		  require(rgdal)
		  read.shape = readOGR(dsn=path.expand(naturalearth.dir),layer=paste0("ne_",reswhat),verbose=verbose)
		  shape2raw = list()
		  shape2raw$type = is(read.shape)[2]
		  if (shape2raw$type == "SpatialLines") {
		    shape2raw$data = list()
		    N.data = 0
		    for (i in 1:length(read.shape@lines)) {
		      for (j in 1:length(read.shape@lines[[i]]@Lines)) {
		        N.data = N.data + 1
		        shape2raw$data[[N.data]] = list(lon=read.shape@lines[[i]]@Lines[[j]]@coords[,1],
		                                         lat=read.shape@lines[[i]]@Lines[[j]]@coords[,2])
		      }
		    }
		  } else if (shape2raw$type == "SpatialPolygons") {
		    shape2raw$split = FALSE
		    shape2raw$data = list()
		    for (i in 1:length(read.shape@polygons)) {
		      for (j in 1:length(read.shape@polygons[[i]]@Polygons)) {
		        lon = read.shape@polygons[[i]]@Polygons[[j]]@coords[,1]
		        lat = read.shape@polygons[[i]]@Polygons[[j]]@coords[,2]
		        shape2raw$data = c(shape2raw$data, list(list(lon=lon,lat=lat,hole=read.shape@polygons[[i]]@Polygons[[j]]@hole)))
		      }
		    }
		  } else if (shape2raw$type == "SpatialPoints") {
		    shape2raw$data = read.shape@data
		    shape2raw$data$lon = read.shape@coords[,1]
		    shape2raw$data$lat = read.shape@coords[,2]
		  } else {
		    stop("Unknown Natural Earth data type.")
		  }
		  saveRDS(shape2raw, fl.r)
		  read.res = shape2raw
		} else {
		  read.res = readRDS(fl.r)
		}
		
		if (read.res$type == "SpatialPolygons" && poly.split && (!file.exists(fl.split) || force.raw2split)) {
		  if (resolution.str == "10m") {
		    warning(paste0("splitted polygons not yet implemented for fine resolution; consider using medium or coarse resolution, set ",
		                   "'poly.split' to FALSE to use complete polygons (which can cause corrupt plots), or use alternative line objects."))
		    read.res = NULL
		  } else {
		    print(paste0("Splitting Natural Earth (filled) polygons into convex subpolygons with sl.polygon.split(), and cutting out holes ",
		                 "(inverse polygons) where present with sl.polygon.mergehole(), which can take a while. ",
		                 "(Required only when used the first time, or forced.)"))
		    N.po = length(read.res$data)
		    print(paste0("object contains ",N.po," polygons"))
		    raw2split = list(type=read.res$type, split=TRUE, data=list())
		    for (j in 1:N.po) {
		      if (read.res$data[[j]]$hole) {next}
		      lon = read.res$data[[j]]$lon
		      lat = read.res$data[[j]]$lat
		      if (j < N.po && read.res$data[[j+1]]$hole) {
		        print(paste0("cutting polyon ",j+1," (hole) out of polygon ",j," and splitting the merged polygon"))
		        lon.hole = read.res$data[[j+1]]$lon
		        lat.hole = read.res$data[[j+1]]$lat
		        poly.mergehole = sl.polygon.mergehole(poly=list(lon=lon,lat=lat),split.costborderbygcdist = FALSE,
		                                              poly.hole = list(lon=lon.hole,lat=lat.hole),
		                                              split.poly = TRUE, checkcross.input = TRUE,
		                                              connect.maxstep = function(x){quantile(x,0.9)})
		        poly.split = c(sl.polygon.split(poly.mergehole[[1]]$lon,poly.mergehole[[1]]$lat,stop.maxiter = Inf,checkcross.input = T,
		                                        split.maxstep = function(x){quantile(x,0.9)},split.cleanup = "keeptripoints",stop.maxangle=pi),
		                       sl.polygon.split(poly.mergehole[[2]]$lon,poly.mergehole[[2]]$lat,stop.maxiter = Inf,checkcross.input = T,
		                                        split.maxstep = function(x){quantile(x,0.9)},split.cleanup = "keeptripoints",stop.maxangle=pi))
		      } else {
		        print(paste0("splitting polyon ",j))
		        poly.split = sl.polygon.split(lon,lat,stop.maxiter = Inf,checkcross.input = F,
		                                      split.maxstep = function(x){quantile(x,0.9)},split.cleanup = "keeptripoints",stop.maxangle=pi)
		      }
		      raw2split$data = c(raw2split$data, poly.split)
		    }
		    saveRDS(raw2split, fl.split)
		    read.res = raw2split
		  }
		}
		
		if (read) {
		  if (!exists("read.res")) {stop("ups, this should not happen")}
		  res.list[[reswhat]] = read.res
		}
		
	}
	
	return(res.list)
	
}
