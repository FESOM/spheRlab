sl.assemble.eulermatrix <- function(al, be, ga, do.d2r = F){
  if(do.d2r){d2r = pi/180} else {d2r = 1}
  
  al = al*d2r; be = be*d2r; ga = ga*d2r
  
  mat = array(dim=c(3,3))
  
  mat[1,1] =  cos(al)*cos(ga) - sin(al)*cos(be)*sin(ga)
  mat[1,2] = -cos(al)*sin(ga) - sin(al)*cos(be)*cos(ga)
  mat[1,3] =  sin(al)*sin(be)
  
  mat[2,1] =  sin(al)*cos(ga) + cos(al)*cos(be)*sin(ga)
  mat[2,2] = -sin(al)*sin(ga) + cos(al)*cos(be)*cos(ga)
  mat[2,3] = -cos(al)*sin(be)
  
  mat[3,1] =  sin(be)*sin(ga) 
  mat[3,2] =  sin(be)*cos(ga)
  mat[3,3] =  cos(be)
  
  return(mat)
}