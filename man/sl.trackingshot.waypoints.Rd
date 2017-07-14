\name{sl.trackingshot.waypoints}
\alias{sl.trackingshot.waypoints}
\title{
  Concatenate waypoints
}
\description{
Concatenates waypoints to a list and extracts and combines their projection field to a toplevel projection field.
The returned list's class is "sl.waypoints".
}
\usage{
sl.trackingshot.waypoints(projection, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{projection}{
    String containing the desired projection to do an equality check with given waypoints.
}
  \item{\dots}{
    lists of class "sl.waypoint" to be concatenated.
}
}
\author{
  Lukas Müller
}
\examples{
  p1 = sl.trackingshot.waypoint(projection = "regpoly", time = 1, regpoly.lonlatrot = c(0,90,0), regpoly.N = 16)
  p2 = sl.trackingshot.waypoint(projection = "regpoly", time = 1, regpoly.lonlatrot = c(15,50,0), regpoly.N = 8)
  sl.trackingshot.waypoints(projection = "regpoly", p1, p2)
}
