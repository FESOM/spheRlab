\name{sl.rot.vec}
\alias{sl.rot.vec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Rotate velocity vectors on a sphere
}
\description{
  Use this function to rotate velocity vectors (given as local u-v-components) on a sphere with Euler angles in z-x-z convention. 
}
\usage{
  sl.rot.vec <- function(al, be, ga, lon, lat, urot, vrot, flag)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{al, be, ga}{
    three numeric scalars defining the Euler angles (in degrees).
  }
  \item{lon, lat}{
    a numeric vector specifying the longitudes and latitudes of the points, either the rotated or geographical coordinates (see parameter 'flag' and section 'Details' for clarification).
  }
  \item{urot, vrot}{
    numeric vectors, the u- and v-component of the velocity vectors that shall be rotated.
  }
  \item{flag}{
    an integer scalar, either 0 or 1, defining how the input coordinates are interpreted. For \code{flag=0}, the input longitude and latitude are considered as the "true" coordinates of the input velocities, and for \code{flag=1} the input longitude and latitude are considered as coordinates that are already rotated with the given Euler angles.
  }

}
\details{
    If for example the velocities are given on longitude and latitude in geographical coordinates, but the vector orientation is still corresponding to the rotated coordinate system, you need to use \code{flag=1} and set the Euler angles to the values that transform the geographic to the rotated coordinates (Example 1 below). If lon and lat are the coordinates of the rotated system, you need to set \code{flag=0}.
}

\value{
  A list with the following elements:
    \item{u}{rotated velocity component (in new eastward direction)}
    \item{v}{rotated velocity components (in new northward direction)}
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  Simon Reifenberg, translated from Python and adapted from FESOM/pyfesom (under MIT license)
}
\note{
  The provided example #1 is also the necessary post-processing for FESOM velocity output.
}

 
  \seealso{
    \code{sl.assemble.eulermatrix}, \code{sl.uvw2uv}, \code{sl.uv2uvw}
  }
\examples{
  ### Example 1: rotated vectors  ->  geographical coordinate system
  #   velocity vectors u, v, are oriented with respect to a North Pole over Greenland (40°W 75°N)
  #   this coordinate rotation was obtained by the Euler angles (50,15,-90)
  #   lon, lat are here the geographical ("real") coordinates
  
  lon = c(0, 30, 60, 90, 180, -10)    # geogr. coord.
  lat = c(0, 30, 45, 60, 80, 85)      # geogr. coord.
  u.rot   = 1.5 * cos(lat*pi/180)     # rotated velocity 
  v.rot   = rep(0, 6)                 # rotated velocity
  
  U.geo <- sl.rot.vec(50, 15, -90, lon, lat, u.rot, v.rot, 1)  # velocity for geogr. coordinate system
  
  ### Example 2: <provided in near future>
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
