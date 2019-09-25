\name{sl.shift}
\alias{sl.shift}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Shift Point on Sphere
}
\description{
Shift a point on a sphere based on zonal and meridional velocities (times dt).
}
\usage{
sl.shift(lon, lat, udt, vdt, Rsphere)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a scalar giving the longitude of the start point.
}
  \item{lat}{
a scalar giving the latitude of the start point.
}
  \item{udt}{
a scalar giving the zonal velocity (times dt). Units should be consistent with the argument \code{Rsphere}.
}
  \item{vdt}{
a scalar giving the meridional velocity (times dt). Units should be consistent with the argument \code{Rsphere}.
}
  \item{Rsphere}{
a scalar giving the radius of the sphere according to which the units of \code{udt} and \code{vdt} are interpreted. Default is \code{1} (unit sphere).
}
}
\details{
The shift is along a great circle that matches the tangent defined by \code{udt} and \code{vdt} at the start location and assuming that \code{udt} and \code{vdt} are valid at the start point; the total 'speed' is then kept constant and is assumed to always point along the great circle. This means that the corresponding 'u' and 'v' computed along the 'trajectory' are actually different from the values provided for the start location.

The computation is carried out linearly in xyz-space (with an appropriate stretching factor to account for the curvature of the great circle); therefore, shifts only up to 90 degree are possible (i.e., \code{sqrt((udt/Rsphere)^2+(vdt/Rsphere)^2) < pi/2}).
}
\value{
\item{lon}{
	a scalar giving the longitude of the shifted point.
}
\item{lat}{
	a scalar giving the latitude of the shifted point.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.p2p}}, \code{\link{sl.fillequidist}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
sl.shift(lon=9,lat=50,u=1000,v=1000,Rsphere=6371)
## Should return:
## $lon
## [1] 26.04508
## 
## $lat
## [1] 57.92084
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
