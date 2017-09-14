\name{sl.trackingshot.way}
\alias{sl.trackingshot.way}
\title{
  Create tracking shot path
}
\description{
  Takes list of class "sl.waypoints" containing all waypoints and a numeric vector of times between the waypoints.
  They are turned into a list of class "sl.way" and returned.
}
\usage{
sl.trackingshot.way(waypoints, times, smooth.line = 0, ignore.checks = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{waypoints}{
    list of class "sl.waypoints" containing waypoints.
}
  \item{times}{
    numeric vector of times between waypoints. Element 1 is the time between waypoints 1 and 2, and so on...
}
  \item{smooth.line}{
    (WIP) logical value indicating if lines should be smoothed.
}
  \item{ignore.checks}{
    logical value indicating if correctness checks should be avoided.
}
}
\author{
  Lukas Müller
}
\examples{
  p1 = sl.trackingshot.waypoint(projection = "polar", time = 1, polar.lonlatrot = c(0,90,0), polar.latbound = 0)
  p2 = sl.trackingshot.waypoint(projection = "polar", time = 1, polar.lonlatrot = c(15,50,0), polar.latbound = 20)
  points = sl.trackingshot.waypoints(projection = "polar", p1, p2)
  sl.trackingshot.way(waypoints = points, times = c(5))
}