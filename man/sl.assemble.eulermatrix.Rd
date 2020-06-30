\name{sl.assemble.eulermatrix}
\alias{sl.assemble.eulermatrix}

\title{
  Assemble rotation matrix for Euler angles (z-x-z convention)
}
\description{
  This function can be used to build the rotation matrix for vector or coordinate rotation by the set of Euler angles which defines the rotation.
}
\usage{
  sl.assemble.eulermatrix(al, be, ga, do.d2r = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{al, be, ga}{ three numeric scalars defining the Euler angles (in radian).}
  \item{do.d2r}{a logical scalar (default: \code{FALSE}), specifying whether the angles are interpreted as radian (\code{FALSE}) or degrees (\code{TRUE}).}  
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns a 3x3 array which can then be used as rotation matrix.
}

\author{
  Simon Reifenberg
}

\examples{
  # note that this example is meant to illustrate how the matrix can be used,
  # the coordinate rotation itself is implemented concisely in sl.rot() 
  
  ### EXAMPLE 
  #   rotate the North Pole to Greenland (-40°E 75°N) - [FESOM standard]
  
  # lon-lat coordinates of North Pole, convert to Cartesian
  NP = c(0,90)                 
  NP.cart = sl.lonlat2xyz(NP)   
  NP.x = NP.cart[1]; NP.y = NP.cart[2]; NP.z = NP.cart[3]
  
  # assemble matrix
  mat = sl.assemble.eulermatrix(50,15,-90, do.d2r=T) 
  
  # matrix multiplication
  NP.x.rot = mat[1,1]*NP.x + mat[1,2]*NP.y + mat[1,3]*NP.z 
  NP.y.rot = mat[2,1]*NP.x + mat[2,2]*NP.y + mat[2,3]*NP.z 
  NP.z.rot = mat[3,1]*NP.x + mat[3,2]*NP.y + mat[3,3]*NP.z 
  
  # convert back to lon-lat coordinates
  NP.rot = sl.xyz2lonlat(x = NP.x.rot, y = NP.y.rot, z = NP.z.rot)
  print(NP.rot)

}

\seealso{
  \code{\link{sl.rot.vec}}, \code{\link{sl.rot}}
}

