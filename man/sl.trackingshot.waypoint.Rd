\name{sl.trackingshot.waypoint}
\alias{sl.trackingshot.waypoint}
\title{
  Create waypoint
}
\description{
  Creates a list with class \code{sl.waypoint} containing necessary arguments for making image(s)
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
\dontrun{
### read mesh
mesh = sl.grid.readFESOM(griddir="somedir",rot=TRUE,rot.invert=TRUE,rot.abg=c(50,15,-90))
# a sample mesh can be obtained from GitHub, e.g., using 'svn checkout' from the command line:
# > svn checkout https://github.com/FESOM/FESOM-data/trunk/pi-grid ~/pi-grid

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