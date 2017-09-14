\name{sl.trackingshot}
\alias{sl.trackingshot}
\title{
Create trackingshot movie
}
\description{
Creates a specified number of temporary images and concatenates them to a video.
}
\usage{
sl.trackingshot(way, num, lon, lat, elem, 
  fps = 25L, time = NA, ffmpeg = Sys.which("ffmpeg"), 
  col.background = "white", col.fill = "colbar", col.border = "colbar", 
  colbar = sl.colbar.redgreyblue_256, colbar.breaks = NA, 
  border.lwd = 0.01, border.lty = 1, file.name = "video.mp4", 
  width = 1080, threads = 1, delete.images = T)
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
}
\author{
  Lukas Müller
}