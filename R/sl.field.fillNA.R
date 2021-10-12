sl.field.fillNA <-
function (num, grd=NULL, max.iter=Inf, method="interative.adjacent.mean", verbose=FALSE) {
	
  N = sl.dim(num)
  if (length(N) > 1) {stop("'num' must be a vector")}
  if (!("neighnodes" %in% names(grd))) {stop("'grd' must contain an element 'neighnodes'")}
  M = sl.dim(grd$neighnodes)
  if (length(M) != 2 || M[1] != N) {stop("'grd$neighnodes' must be a matrix with length('num') rows")}
  M = M[2]
  
  sum.na = sum(is.na(num))
  if (sum.na == N) {stop("'num' contains NA values only and can thus not be filled")}
  
  iter = 0
  while (sum.na > 0) {
    
    if (iter >= max.iter) {
      warning(paste0(sum.na," NA values remaining after 'max.iter'=",iter," iterations; returning partially NA-filled data"))
      break
    }
    iter = iter + 1
    
    if (verbose) {print(paste0(sum.na," NA values remaining; filling iteration ",iter, " ..."))}
    num.withgaps = num
    
    if (method == "interative.adjacent.mean") {
      for (i in which(is.na(num.withgaps))) {
        num[i] = mean(num.withgaps[grd$neighnodes[i,which(!is.na(grd$neighnodes[i,]))]], na.rm=TRUE)
      }
    } else {
      stop("NA fill methods other than 'interative.adjacent.mean' not implemented")
    }
    
    sum.na.previous = sum.na
    sum.na = sum(is.na(num))
    if (sum.na == sum.na.previous) {
      warning(paste0(sum.na," NA values can not be filled (located in disjunct NA-only grid domains); returning partially NA-filled data"))
      break
    }
    
  }
  
  if (verbose && sum.na == 0) {print("All NA values successfully filled")}
  return(num)
	
}
