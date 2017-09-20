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
sl.plot.lonlatgrid(plot.init.res, lon.0 = 0, lat.0 = 0, lon.distance = 10, lat.distance = lon.distance, pole.hole = FALSE, precision = 1, lty = 1, lwd = 0.25, col = "black")
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
a scalar specifying the distance (degrees) between adjacent longitudes.
}
  \item{lat.distance}{
a scalar specifying the distance (degrees) between adjacent latitudes.
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
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.plot.naturalearth}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
