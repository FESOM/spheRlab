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
