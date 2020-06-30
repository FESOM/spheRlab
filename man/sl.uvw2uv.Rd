\name{sl.uvw2uv}
\alias{sl.uvw2uv}
\title{
Coordinate transformation of velocity vectors (3D -> 2D)
}
\description{
Convert velocities from u-v-w coordinates (3D) on the unit sphere to u-v-components in local coordinate system (2D).
}
\usage{
sl.uv2uvw(u, v, w, lon, lat, do.d2r = TRUE)
}

\arguments{
  \item{u,v,w}{numerical vectors (or scalars) for the velocity components.}
  \item{lon,lat}{numerical vectors (or scalars) of longitudes (\code{lon}) and latitudes (\code{lat}) of the velocity vectors.}
  \item{do.d2r}{a logical scalar (default: \code{TRUE}), specifying whether longitude and latitude are interpreted as radian (\code{FALSE}) or degrees (\code{TRUE}).}
  
}

\value{
Returns a list containing
\item{u}{ first converted velocity component in local coordinate system}
\item{v}{ second converted velocity component in local coordinate system}
}

\author{
Simon Reifenberg
}
\note{
The inverse transformation is implemented in \code{\link{sl.uv2uvw}}.
}

\seealso{
  \code{\link{sl.uv2uvw}}
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