sl.tracer.get.nodecoords.of.triangle <- function (tri=NULL, triangles=NULL, points, sub2full = NULL) {
  # _tri_ should be a set of three parameters (node indices) or a (sliced) triangle matrix 3xL
  if (!is.null(tri)){
    if (is.null(triangles)) {
      return(list(x1 = points[,tri[1]], x2 = points[,tri[2]], x3 = points[,tri[3]]))
    } else {warning("'tri' and 'triangles' can't be used at the same time, aborting process")}
  } else {
    if (!is.null(triangles)) {
      L_ = length(triangles[,1])
      x1 = matrix(nrow = 2, ncol = L_)
      x2 = matrix(nrow = 2, ncol = L_)
      x3 = matrix(nrow = 2, ncol = L_)
      for (l in 1:L_) {
        tri = triangles[l,1:3]
        if (!is.null(sub2full)){
          x1[,l] = points[,sub2full[tri[1]]]
          x2[,l] = points[,sub2full[tri[2]]]
          x3[,l] = points[,sub2full[tri[3]]]
        }
        else {
          x1[,l] = points[,tri[1]]
          x2[,l] = points[,tri[2]]
          x3[,l] = points[,tri[3]]
        }
      }
      return(list(x1=x1, x2=x2, x3=x3))
    } else {warning("either 'tri' or 'triangles' has to be given")}
  }
}
