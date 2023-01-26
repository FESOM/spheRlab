sl.num2colbar <-
function (num,colbar=NULL,breaks=NA,breaks.log=FALSE) {
	
  if (!is.null(colbar) && !is.null(names(colbar)) && "colbar" %in% names(colbar)) {
    if ("categorical" %in% names(colbar)) {categorical = colbar$categorical}
    if ("breaks" %in% names(colbar)) {breaks = colbar$breaks}
    colbar = colbar$colbar
  }
  
	if (is.null(breaks) || anyNA(breaks)) {
	  if (is.null(colbar)) {stop("'breaks' are not provided or contain NAs, so 'colbar' is required")}
		breaks = sl.num2colbarbreaks(num,colbar,breaks.log=breaks.log)
	}
	Nbreaks = length(breaks)
	col.ind = num * 0 + 1
	not.na = !is.na(num)
	col.ind[!not.na] = NA
	for (i in 1:Nbreaks) {
		col.ind[not.na & num>breaks[i]] = col.ind[not.na & num>breaks[i]] + 1
	}
	return(list(colour.index=col.ind,breaks=breaks,colbar=colbar))
	
}
