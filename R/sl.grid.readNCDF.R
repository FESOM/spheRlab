sl.grid.readNCDF <-
function (filename) {
	
  require(ncdf4)
  
  fl = nc_open(filename)
  vars = names(fl$var)
  
  res = list()
  
  i = 0
  for (var in vars[vars!="const"]) {
    i = i + 1
    res[[i]] = ncvar_get(fl,varid=var)
  }
  names(res) = vars[vars!="const"]
  
  if ("triag_nodes" %in% names(res)) {
    names(res)[names(res)=="triag_nodes"] = "elem"
    res$elem = t(res$elem)
  }
  names(res)[names(res)=="lon_vertices"] = "lon_bnds"
  names(res)[names(res)=="lat_vertices"] = "lat_bnds"
	
	return(res)
	
}