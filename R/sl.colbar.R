sl.colbar <-
function (cols,N=(length(cols)-1)*10+1,cols.at=seq(0,1,by=1/(length(cols)-1))) {
	
  Ncols = length(cols)
  
  if (is.character(cols)) {
    cols.in = cols
    cols = list()
    for (i in 1:Ncols) {
      cols[[i]] = as.vector(col2rgb(cols.in[i]))/255
    }
  }
	
	col.out = list()
	col.out[[1]] = rgb(matrix(cols[[1]],ncol=3))
	for (i in 2:(N-1)) {
		prog = (i-1)/(N-1)
		c2 = match(TRUE,prog<cols.at)
		c1 = c2 - 1
		col1 = cols[[c1]]
		col2 = cols[[c2]]
		frac = (prog - cols.at[c1]) / (cols.at[c2] - cols.at[c1])
		col.out[[i]] = rgb(matrix((1-frac)*col1+frac*col2,ncol=3))
	}
	col.out[[N]] = rgb(matrix(cols[[Ncols]],ncol=3))
	
	return(col.out)
	
}
