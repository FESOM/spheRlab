sl.load.naturalearth <-
function (what="all",resolution="medium",naturalearth.dir="~/naturalearthdata",force.download=FALSE,download.if.missing=TRUE,download.baseurl="http://www.naturalearthdata.com/http//www.naturalearthdata.com/download",read=TRUE,verbose=TRUE) {
	
	if (length(what) == 1 && (what == "all" || what == "list" )) {
		if (what == "all" && length(resolution) > 1) {stop("what='all' works only for one resolution at a time.")}
		what.orig = what
		if (resolution[1] == "coarse") {
			what = c("coastline","geographic_lines","geography_marine_polys","geography_regions_elevation_points","geography_regions_points","geography_regions_polys","glaciated_areas","graticules_1","graticules_5","graticules_10","graticules_15","graticules_20","graticules_30","lakes","land","ocean","rivers_lake_centerlines","wgs84_bounding_box")
		} else if (resolution[1] == "medium") {
			what = c("antarctic_ice_shelves_lines","antarctic_ice_shelves_polys","coastline","geographic_lines","geography_marine_polys","geography_regions_elevation_points","geography_regions_points","geography_regions_polys","glaciated_areas","graticules_1","graticules_5","graticules_10","graticules_15","graticules_20","graticules_30","lakes_historic","lakes","land","ocean","playas","rivers_lake_centerlines_scale_rank","rivers_lake_centerlines","wgs84_bounding_box")
		} else if (resolution[1] == "fine") {
			what = c("antarctic_ice_shelves_lines","antarctic_ice_shelves_polys","bathymetry_A_10000","bathymetry_B_9000","bathymetry_C_8000","bathymetry_D_7000","bathymetry_E_6000","bathymetry_F_5000","bathymetry_G_4000","bathymetry_H_3000","bathymetry_I_2000","bathymetry_J_1000","bathymetry_K_200","bathymetry_L_0","coastline","geographic_lines","geography_marine_polys","geography_regions_elevation_points","geography_regions_points","geography_regions_polys","glaciated_areas","graticules_1","graticules_5","graticules_10","graticules_15","graticules_20","graticules_30","lakes_europe","lakes_historic","lakes_north_america","lakes_pluvial","lakes","land_ocean_label_points","land_ocean_seams","land_scale_rank","land","minor_islands_coastline","minor_islands_label_points","minor_islands","ocean_scale_rank","ocean","playas","reefs","rivers_europe","rivers_lake_centerlines_scale_rank","rivers_lake_centerlines","rivers_north_america","wgs84_bounding_box")
		} else {stop("'resolution' must be one of 'coarse', 'medium', and 'fine'.")}
		if (what.orig == "list") {
			print(paste0("The following 'what' values are available for resolution='",resolution,"':"))
			print(what)
			return(NULL)
		}
	}
	
	if (read) {require(rgdal)}
	
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
		
		if (read) {
			res.list[[reswhat]] = readOGR(dsn=path.expand(naturalearth.dir),layer=paste0("ne_",reswhat),verbose=verbose)
		}
		
	}
	
	return(res.list)
	
}
