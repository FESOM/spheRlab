sl.num2colbarbreaks <-
function (num,colbar=NULL,Nbreaks=NULL,breaks.log=FALSE) {
	
  if (is.null(Nbreaks)) {
	  if (!is.null(colbar)) {
		  Nbreaks = length(colbar) - 1
	  } else {
	    Nbreaks = 10
	  }
  }
	if (breaks.log) {
		max.num = log(max(num,na.rm=TRUE))
		min.num = log(min(num[num>0],na.rm=TRUE))
		if (is.na(min.num)) {stop("no positive values contained, no logarithmic breaks possible")}
	} else {
	  max.num = max(num,na.rm=TRUE)
	  min.num = min(num,na.rm=TRUE)
	}
	stp = (max.num - min.num) / (Nbreaks + 1)
	breaks = seq(min.num+stp,max.num-stp/2,stp)
	if (breaks.log) {
		breaks = exp(breaks)
	}
	return(breaks)
	
}
