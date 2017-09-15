\name{sl.trackingshot}
\alias{sl.trackingshot}
\title{
Create Trackingshot Movie
}
\description{
Creates a specified number of temporary images and concatenates them to a video. SEE NOTE 1) BELOW FOR A WORKAROUND FOR AN EXISTING ISSUE.
}
\usage{
sl.trackingshot(way, num, lon, lat, elem, 
  fps = 25L, time = NA, ffmpeg = Sys.which("ffmpeg"), 
  col.background = "white", col.fill = "colbar", col.border = "colbar", 
  colbar = sl.colbar.redgreyblue_256, colbar.breaks = NA, 
  border.lwd = 0.01, border.lty = 1, out.path = getwd(), file.name = "sl.trackingshot.mp4", 
  width = 1080, threads = 1, delete.images = TRUE, verbose = TRUE)
}
\arguments{
  \item{way}{
    list of class "sl.way" like the one returned by \code{\link{sl.trackingshot.way}}
}
  \item{num, lon, lat, elem}{
    Corresponding argument to \code{\link{sl.plot.field.elem}}
}
  \item{fps}{
    scalar representing the video's frames per second. Must be greater than 0.
}
  \item{time}{
    Scalar or NA. If NA the way's times will be used, otherwise it will be stretched to time's size.
}
  \item{ffmpeg}{
    String. Path to ffmpeg executable.
}
  \item{col.background}{
    Image background
}
  \item{col.fill}{
    Fill color for polygons
}
  \item{col.border}{
    Polygon border color
}
  \item{colbar}{
    List containing color codes.
}
  \item{colbar.breaks}{
    %%     ~~Describe \code{colbar.breaks} here~~
}
  \item{border.lwd}{
    Border line width
}
  \item{border.lty}{
    Border line type
}
  \item{out.path}{
    Output path for the video file (and the folder with the temporary images)
}
  \item{file.name}{
    File name for the video file
}
  \item{width}{
    Base width for all images in pixels
}
  \item{threads}{
    Thread count to use for creating images
}
  \item{delete.images}{
    Logical value indicating if produced images should be deleted after the video is created
}
  \item{verbose}{
    Logical value indicating whether additional info is printed
}
}
\note{
1) It appears that the function fails if it's immediately executed with multiple threads. This can be circumvented by starting the function with a single thread, aborting it (if more threads are desired), and re-running it with multiple threads.

2) This initial implementation of the function is designed to make movies based on one specific plotting function, namely \code{\link{sl.plot.field.elem}}. For the future it would be nice to make this tool more general. For example, the trackingshot function could take a function containing arbitrary plot calls as an argument. The user could then define such a function quite generally, e.g., adding coastlines, points, or whatsoever. Ideally, it should also be possible to use data with a time dimension, so that not only the view is changing but at the same time the displayed field advances in time.

}
\author{
  Lukas Müller, slightly modified by Helge Goessling
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