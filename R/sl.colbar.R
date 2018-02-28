sl.colbar <-
function (cols,N=(length(cols)-1)*10+1,cols.at=seq(0,1,by=1/(length(cols)-1))) {
	
  Ncols = length(cols)
  
  cols.in = cols
  cols = list()
  for (i in 1:Ncols) {
    cols[[i]] = as.vector(col2rgb(cols.in[i],alpha=TRUE))/255
  }
	
	col.out = list()
	col.out[[1]] = rgb(cols[[1]][1],cols[[1]][2],cols[[1]][3],cols[[1]][4])
	for (i in 2:(N-1)) {
		prog = (i-1)/(N-1)
		c2 = match(TRUE,prog<cols.at)
		c1 = c2 - 1
		col1 = cols[[c1]]
		col2 = cols[[c2]]
		frac = (prog - cols.at[c1]) / (cols.at[c2] - cols.at[c1])
		col.out.tmp = (1-frac)*col1+frac*col2
		col.out[[i]] = rgb(col.out.tmp[1],col.out.tmp[2],col.out.tmp[3],col.out.tmp[4])
	}
	col.out[[N]] = rgb(cols[[Ncols]][1],cols[[Ncols]][2],cols[[Ncols]][3],cols[[Ncols]][4])
	
	return(col.out)
	
}
