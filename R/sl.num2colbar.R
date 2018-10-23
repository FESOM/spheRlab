sl.num2colbar <-
function (num,colbar=sl.colbar.blackwhite_256,breaks=NA,breaks.log=FALSE) {
	
  if (!is.null(names(colbar)) && "colbar" %in% names(colbar)) {
    if ("categorical" %in% names(colbar)) {categorical = colbar$categorical}
    if ("breaks" %in% names(colbar)) {breaks = colbar$breaks}
    colbar = colbar$colbar
  }
  
	if (anyNA(breaks)) {
		breaks = sl.num2colbarbreaks(num,colbar,breaks.log=breaks.log)
	}
	Nbreaks = length(breaks)
	col.ind = num * 0 + 1
	not.na = !is.na(num)
	col.ind[!not.na] = NA
	for (i in 1:Nbreaks) {
		col.ind[not.na & num>breaks[i]] = col.ind[not.na & num>breaks[i]] + 1
	}
	return(list(colour.index=col.ind,breaks=breaks))
	
}
