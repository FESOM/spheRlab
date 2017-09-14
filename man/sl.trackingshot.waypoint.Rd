\name{sl.trackingshot.waypoint}
\alias{sl.trackingshot.waypoint}
\title{
  Create waypoint
}
\description{
  Creates a list with class "sl.waypoint" containing necessary arguments for making image(s)
}
\usage{
sl.trackingshot.waypoint(projection, time = 0, 
  lonlat.lonrange = c(-180, 180), lonlat.latrange = c(-85, 85), 
  polar.lonlatrot = c(0, 90, 0), polar.latbound = 0, 
  regpoly.lonlatrot = c(0, 90, 0), regpoly.N = 3, 
  regpoly.lat0 = 60, regpoly.rotfrac = 0)
}
\arguments{
  \item{projection}{
    string indicating which projection type to use
}
  \item{time}{
    scalar for how long the camera should stay at this waypoint. Must be greater than or equals 0.
}
  \item{lonlat.lonrange, lonlat.latrange, polar.lonlatrot, polar.latbound, regpoly.lonlatrot, regpoly.N, regpoly.lat0, regpoly.rotfrac}{
    The corresponding arguments to \code{\link{sl.plot.init}}
  }
}
\author{
  Lukas Müller
}
\examples{
  sl.trackingshot.waypoint(projection = "lonlat", time = 1, lonlat.lonrange = c(-180,180), lonlat.latrange = c(-85,85))
}