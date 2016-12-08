sl.num2colbar <-
function (num,colbar=sl.colbar.blackwhite_256,breaks=NA,breaks.log=FALSE) {
	
	if (anyNA(breaks)) {
		breaks = sl.num2colbarbreaks(num,colbar,breaks.log=breaks.log)
	}
	Nbreaks = length(breaks)
	col.ind = num * 0 + 1
	for (i in 1:Nbreaks) {
		col.ind[num>breaks[i]] = col.ind[num>breaks[i]] + 1
	}
	return(list(colour.index=col.ind,breaks=breaks))
	
}
