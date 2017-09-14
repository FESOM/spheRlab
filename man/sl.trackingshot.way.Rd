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
\dontrun{
### read mesh
mesh = sl.grid.readFESOM(griddir="somedir",rot=TRUE,rot.invert=TRUE,rot.abg=c(50,15,-90))

### read corresponding data
require(ncdf4)
dat.file = nc_open(filename="somecorrespondingdata.nc")
dat = ncvar_get(nc=dat.file)
nc_close(nc=dat.file)

### define projection and way
projection = "polar"
p1 = sl.trackingshot.waypoint(projection = projection, time = 1, polar.lonlatrot = c(0,90,0), polar.latbound = 0)
p2 = sl.trackingshot.waypoint(projection = projection, time = 1, polar.lonlatrot = c(15,50,0), polar.latbound = 20)
p3 = sl.trackingshot.waypoint(projection = projection, time = 1, polar.lonlatrot = c(-60,-30,0), polar.latbound = 60)
p4 = sl.trackingshot.waypoint(projection = projection, time = 1, polar.lonlatrot = c(100,40,0), polar.latbound = 20)
p5 = p1
p5$time = 0
points = sl.trackingshot.waypoints(projection = projection, p1, p2, p3, p4, p5)
way = sl.trackingshot.way(waypoints = points, times = c(5,5,5,5))

### make video
sl.trackingshot(way = way, num = dat, lon = mesh$lon, lat=mesh$lat, elem=mesh$elem, fps = 30L, width = 1920, threads = 3, file.name = paste0("~/video_", projection, ".mp4"))
}
}