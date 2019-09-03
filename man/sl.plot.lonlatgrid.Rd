\name{sl.plot.lonlatgrid}
\alias{sl.plot.lonlatgrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Longitude-Latitude Grid
}
\description{
Plot a simple longitude-latitude grid over a spheRlab plot.
}
\usage{
sl.plot.lonlatgrid(plot.init.res, lon.0 = 0, lat.0 = 0, lon.distance = NULL, lat.distance = NULL, lon.range = c(-180,180), lat.range = c(-90,90), autodensity.f = 1, pole.hole = FALSE, precision = 1, lty = 1, lwd = 0.25, col = "black", labels = FALSE, labels.lat.every = 3, labels.lat.first = ceiling(labels.lat.every/2), labels.lat.offsetlatlon = c(0,0), labels.lon.every = 3, labels.lon.first = ceiling(labels.lon.every/2), labels.lon.offsetlatlon = c(0,0), labels.col = "grey", labels.cex = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{\link{sl.plot.init}} (or a variant thereof).
}
  \item{lon.0}{
a scalar specifying one of the longitudes (degrees east) to be drawn.
}
  \item{lat.0}{
a scalar specifying one of the latitudes (degrees north) to be drawn.
}
  \item{lon.distance}{
a scalar specifying the distance (degrees) between adjacent longitudes. Default is \code{NULL}, in which case a reasonable value is found automatically (can be further adjusted with the argument \code{autodensity.f}.)
}
  \item{lat.distance}{
a scalar specifying the distance (degrees) between adjacent longitudes. Default is \code{NULL}, in which case a reasonable value is found automatically (can be further adjusted with the argument \code{autodensity.f}.)
}
  \item{lon.range}{
a two-element numeric vector specifying the longitude range in which grid lines are to be drawn. Default is \code{c(-180,180)}.
}
  \item{lat.range}{
a two-element numeric vector specifying the latitude range in which grid lines are to be drawn. Default is \code{c(-90,90)}.
}
  \item{autodensity.f}{
a scalar that can be used to adjust the density of automatically derived values for \code{lat.distance} and/or \code{lon.distance} (used only if the latter are \code{NULL}). Default is \code{autodensity.f=1}.
}
  \item{pole.hole}{
a logical value indicating whether or not to let longitudinal lines end at the last latitude(s) before the pole(s).
}
  \item{precision}{
a scalar specifying the precision (degrees) at which longitudes and latitudes are broken into line segments (which are drawn as straight lines).
}
  \item{lty}{
a line type.
}
  \item{lwd}{
a scalar specifying the line width.
}
  \item{col}{
a line colour.
}
  \item{labels}{
a logical value indicating whether or not to add longitude and latitude labels. Note that the placement of labels is determined automatically based on the lon-lat-grid, with some arguments to control the behaviour. Latitude labels are placed between lines of longitudes and vice versa. For more flexibility, use \code{sl.plot.lonlatlabels} directly.
}
  \item{labels.lat.every}{
an integer specifying the density in latitudinal direction at which labels are plotted. Affects latitude and longitude labels coherently.
}
  \item{labels.lat.first}{
an integer specifying the first latitude to receive a latitude label. Affects latitude and longitude labels coherently such that longitude labels are placed between latitude labels.
}
  \item{labels.lat.offsetlatlon}{
a 2-element numeric vector specifying the degrees latitude and longitude by which latitude labels are to be shifted.
}
  \item{labels.lon.every}{
an integer specifying the density in longitudinal direction at which labels are plotted. Affects latitude and longitude labels coherently.
}
  \item{labels.lon.first}{
an integer specifying the first longitude to receive a longitude label. Affects longitude and latitude labels coherently such that latitude labels are placed between longitude labels.
}
  \item{labels.lon.offsetlatlon}{
a 2-element numeric vector specifying the degrees latitude and longitude by which longitude labels are to be shifted.
}
  \item{labels.col}{
the color to be used for the labels.
}
  \item{labels.cex}{
numeric character expansion factor for the labels; multiplied by \code{par("cex")} yields the final character size. \code{NULL} and \code{NA} are equivalent to \code{1.0}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
Some standard lon-lat grids can also be plotted using \code{\link{sl.plot.naturalearth}}.

If you set \code{labels=TRUE}, note that the placement of labels is determined automatically based on the lon-lat-grid, with few arguments to control the behaviour. For more flexibility, use \code{sl.plot.lonlatlabels} directly.

If grid lines are very dense, this function can be quite slow even if only a small map section is actually plotted. In such cases, the arguments \code{lon.range} and \code{lat.range} can be used to considerably speed up the process.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.plot.naturalearth}}, \code{\link{sl.plot.lonlatlabels}}
}
\examples{
\dontrun{
pir = sl.plot.init()
sl.plot.naturalearth(pir, resolution="coarse")
sl.plot.lonlatgrid(pir, labels=TRUE, labels.lat.first=3, labels.lon.first=1, labels.cex=0.7, labels.col="red")
sl.plot.end(pir)
### Should result in a pdf file named 'sl.plot.pdf' in your home directory.
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
