sl.num2colbarbreaks <-
function (num,colbar=NULL,Nbreaks=NULL,breaks.log=FALSE) {
	
  if (is.null(Nbreaks)) {
	  if (!is.null(colbar)) {
		  Nbreaks = length(colbar) - 1
	  } else {
	    Nbreaks = 10
	  }
  }
	max.num = max(num,na.rm=TRUE)
	min.num = min(num,na.rm=TRUE)
	if (breaks.log) {
		max.num = log(max.num)
		min.num = log(min.num)
	}
	stp = (max.num - min.num) / (Nbreaks + 1)
	breaks = seq(min.num+stp,max.num-stp/2,stp)
	if (breaks.log) {
		breaks = exp(breaks)
	}
	return(breaks)
	
}
