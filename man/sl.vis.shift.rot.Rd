\name{sl.vis.shift.rot}
\alias{sl.vis.shift.rot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check Visibility, Shift, and Rotate Points
}
\description{
Check the visibility, shift, and rotate points for a spheRlab plot.
}
\usage{
sl.vis.shift.rot(plot.init.res, lon, lat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{sl.plot.init} (or a variant thereof).
}
  \item{lon}{
a vector of length \code{N} specifying the longitudes of the points.
}
  \item{lat}{
a vector of length \code{N} specifying the latitudes of the points.
}
}
\details{
This function is intended mainly for internal usage.
}
\value{
A list with the following elements:
  \item{x}{
a vector of length \code{N} specifying the plot-projection x-coordinates of the points.
}
  \item{y}{
a vector of length \code{N} specifying the plot-projection y-coordinates of the points.
}
  \item{rot.lon}{
a vector of length \code{N} specifying the post-rotation longitudes of the points.
}
  \item{rot.lat}{
a vector of length \code{N} specifying the post-rotation latitudes of the points.
}
  \item{visible}{
a vector of length \code{N} with logical values indicating the visibility of the points.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
This function is used by \code{sl.plot.lines}, \code{sl.plot.polygon}, and \code{sl.plot.text}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
%
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
