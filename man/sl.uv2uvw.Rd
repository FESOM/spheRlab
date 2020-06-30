\name{sl.uv2uvw}
\alias{sl.uv2uvw}
\title{
Coordinate transformation of velocity vectors (2D -> 3D)
}
\description{
Convert velocities from local coordinate system (2D) to u-v-w coordinates (3D) on the unit sphere.
}
\usage{
sl.uv2uvw(u, v, lon, lat, do.d2r = TRUE)
}

\arguments{
  \item{u,v}{numerical vectors (or scalars) for the velocity components in zonal (\code{u}) and meridional (\code{v}) direction.}
  \item{lon,lat}{numerical vectors (or scalars) of longitudes (\code{lon}) and latitudes (\code{lat}) of the velocity vectors.}
  \item{do.d2r}{a logical scalar (default: \code{TRUE}), specifying whether longitude and latitude are interpreted as radian (\code{FALSE}) or degrees (\code{TRUE}).}
  
}
\details{
Can be used to transform the u-v vectors in the local "eastward-northward" coordinate system on a sphere into three-dimensional vectors. This facilitates the corresponding vector rotation for a given coordinate rotation.
}
\value{
Returns a list containing
\item{u}{ first converted velocity component}
\item{v}{ second converted velocity component}
\item{w}{ third converted velocity component}

%% ...
}

\author{
Simon Reifenberg
}
\note{
The inverse transformation is implemented in \code{\link{sl.uvw2uv}}.
}

\seealso{
  \code{\link{sl.uvw2uv}}
}
\examples{
# define local velocity vectors at three positions
lon = c(0, 45, 90)
lat = c(0, 45,  0)
u   = c(0, sqrt(2)/2, 1)
v   = c(1, sqrt(2)/2, 0)

# convert to 'global' coordinates
U <- sl.uv2uvw(u, v, lon, lat)
print(U)


# convert back to local coordinates
U.loc <- sl.uvw2uv(U$u, U$v, U$w, lon, lat, do.d2r = T)
print(U.loc)

}