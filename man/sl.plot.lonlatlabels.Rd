\name{sl.plot.lonlatlabels}
\alias{sl.plot.lonlatlabels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Longitude-Latitude Labels
}
\description{
Plot longitude and latitude labels over a spheRlab plot.
}
\usage{
sl.plot.lonlatlabels(plot.init.res, lat.lons = NULL, lat.lats = NULL, lon.lons = NULL, lon.lats = NULL, col = "grey", cex = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{\link{sl.plot.init}} (or a variant thereof).
}
  \item{lat.lons}{
a numeric vector specifying at which longitudes (degrees east) latitude labels are to be drawn.
}
  \item{lat.lats}{
a numeric vector specifying at which latitudes (degrees north) latitude labels are to be drawn.
}
  \item{lon.lons}{
a numeric vector specifying at which longitudes (degrees east) longitude labels are to be drawn.
}
  \item{lon.lats}{
a numeric vector specifying at which latitudes (degrees north) longitude labels are to be drawn.
}
  \item{col}{
the color to be used for the labels, possibly a vector.
}
  \item{cex}{
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
This function is used by \code{sl.plot.lonlatgrid} if \code{labels=TRUE} is set in that function, with a simple way to determine the placement of labels automatically and with few arguments to control the behaviour. For more flexibility, this function can be used directly.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.plot.lonlatgrid}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
