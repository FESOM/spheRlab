sl.remap.array <- function (input.dimvals,input.data,new.dimvals,remap.dim=1,method="linear",extrapolate=FALSE,return.remapinfo=FALSE,verbose=TRUE) {
  
  if (method != "nearestneighbour" && method != "linear") {
    stop("'method' must be one of 'nearestneighbour' and 'linear'.")
  }
  
  input.N = length(input.dimvals)
  new.N = length(new.dimvals)
  input.dim = dim(input.data)
  if (is.null(input.dim)) {input.data = array(input.data); input.dim = dim(input.data)}   # convert vector into 1D array
  ND = length(input.dim)
  if (ND > 4) {stop("'input.data' must not have more than 4 dimensions")}
  if (remap.dim > ND) {stop("'remap.dim' is larger than the number of dimensions of 'input.data'")}
  if (input.N != input.dim[remap.dim]) {stop("length of 'input.dimvals' is inconsistent with 'input.data' and 'remap.dim'")}
  new.dim = input.dim
  new.dim[remap.dim] = new.N
  
  if (new.N > 1) {
    if (any(new.dimvals[2:new.N] <= new.dimvals[1:(new.N-1)])) {stop("'new.dimvals' must increase strict monotonously")}
  }
  if (input.N <= 1) {stop("'input.dimvals' must have at least two elements")}
  if (any(input.dimvals[2:input.N] <= input.dimvals[1:(input.N-1)])) {stop("'input.dimvals' must increase strict monotonously")}
    
  if (new.dimvals[new.N] > input.dimvals[input.N]) {
    if (new.dimvals[1] > input.dimvals[input.N]) {
      if (verbose) {warning("New dimension values completely out of original dimension values.")}
      if (!extrapolate) {
        if (verbose) {warning("All values will be 'NA'.")}
      } else {
        if (verbose) {warning("All values will be extrapolated.")}
      }
    } else {
      if (verbose) {warning("Maximum of new dimension values above maximum of original dimension values.")}
      if (extrapolate) {
        if (verbose) {warning("Values outside original dimension range will be extrapolated.")}
      } else {
        if (verbose) {warning("Values outside original dimension range will be 'NA'.")}
      }
    }
  }
  if (new.dimvals[1] < input.dimvals[1]) {
    if (new.dimvals[new.N] < input.dimvals[1]) {
      if (verbose) {warning("New dimension values completely out of original dimension values.")}
      if (!extrapolate) {
        if (verbose) {warning("All values will be 'NA'.")}
      } else {
        if (verbose) {warning("All values will be extrapolated.")}
      }
    } else {
      if (verbose) {warning("Minimum of new dimension values below minimum of original dimension values.")}
      if (extrapolate) {
        if (verbose) {warning("Values outside original dimension range will be extrapolated.")}
      } else {
        if (verbose) {warning("Values outside original dimension range will be 'NA'.")}
      }
    }
  }
  
  weights.left = rep(NA,new.N)
  weights.left.ind = rep(NA,new.N)
  i.input = 1
  for (i.new in 1:new.N) {
    while (i.input < (input.N-1) && new.dimvals[i.new] > input.dimvals[i.input+1]) {
      i.input = i.input + 1
    }
    weights.left.ind[i.new] = i.input
    weights.left[i.new] = (input.dimvals[i.input+1] - new.dimvals[i.new]) / (input.dimvals[i.input+1] - input.dimvals[i.input])
  }
  if (!extrapolate) {
    weights.left[weights.left < 0 | weights.left > 1] = NA
  }
  if (method == "nearestneighbour") {
    weights.left[weights.left > .5] = 1
    weights.left[weights.left <= .5] = 0
  }
  
  new.data = array(dim=new.dim)
  if (ND == 1) {
    for (i.new in 1:new.N) {
      wli = weights.left.ind[i.new]
      wl = weights.left[i.new]
      new.data[i.new] = (input.data[wli]*wl + input.data[wli+1]*(1-wl))
    }
  }
  if (ND == 2 && remap.dim == 1) {
    for (i.new in 1:new.N) {
      wli = weights.left.ind[i.new]
      wl = weights.left[i.new]
      new.data[i.new,] = (input.data[wli,]*wl + input.data[wli+1,]*(1-wl))
    }
  }
  if (ND == 2 && remap.dim == 2) {
    for (i.new in 1:new.N) {
      wli = weights.left.ind[i.new]
      wl = weights.left[i.new]
      new.data[,i.new] = (input.data[,wli]*wl + input.data[,wli+1]*(1-wl))
    }
  }
  if (ND == 3 && remap.dim == 1) {
    for (i.new in 1:new.N) {
      wli = weights.left.ind[i.new]
      wl = weights.left[i.new]
      new.data[i.new,,] = (input.data[wli,,]*wl + input.data[wli+1,,]*(1-wl))
    }
  }
  if (ND == 3 && remap.dim == 2) {
    for (i.new in 1:new.N) {
      wli = weights.left.ind[i.new]
      wl = weights.left[i.new]
      new.data[,i.new,] = (input.data[,wli,]*wl + input.data[,wli+1,]*(1-wl))
    }
  }
  if (ND == 3 && remap.dim == 3) {
    for (i.new in 1:new.N) {
      wli = weights.left.ind[i.new]
      wl = weights.left[i.new]
      new.data[,,i.new] = (input.data[,,wli]*wl + input.data[,,wli+1]*(1-wl))
    }
  }
  if (ND == 4 && remap.dim == 1) {
    for (i.new in 1:new.N) {
      wli = weights.left.ind[i.new]
      wl = weights.left[i.new]
      new.data[i.new,,,] = (input.data[wli,,,]*wl + input.data[wli+1,,,]*(1-wl))
    }
  }
  if (ND == 4 && remap.dim == 2) {
    for (i.new in 1:new.N) {
      wli = weights.left.ind[i.new]
      wl = weights.left[i.new]
      new.data[,i.new,,] = (input.data[,wli,,]*wl + input.data[,wli+1,,]*(1-wl))
    }
  }
  if (ND == 4 && remap.dim == 3) {
    for (i.new in 1:new.N) {
      wli = weights.left.ind[i.new]
      wl = weights.left[i.new]
      new.data[,,i.new,] = (input.data[,,wli,]*wl + input.data[,,wli+1,]*(1-wl))
    }
  }
  if (ND == 4 && remap.dim == 4) {
    for (i.new in 1:new.N) {
      wli = weights.left.ind[i.new]
      wl = weights.left[i.new]
      new.data[,,,i.new] = (input.data[,,,wli]*wl + input.data[,,,wli+1]*(1-wl))
    }
  }
  
  if (return.remapinfo) {
    return(list(data=new.data,remapinfo=list(weights.left.ind=weights.left.ind,weights.left=weights.left)))
  } else {
    return(list(data=new.data))
  }
  
  
}