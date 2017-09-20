\name{sl.lonlat.angle}
\alias{sl.lonlat.angle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Angle (GC-Distance) between Points on Sphere
}
\description{
Compute the angle (great-circle distance) between two points on a sphere.
}
\usage{
sl.lonlat.angle(lon1, lat1, lon2, lat2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon1}{
  a scalar giving the longitude of the first point.
}
  \item{lat1}{
  a scalar giving the latitude of the first point.
}
  \item{lon2}{
  a scalar giving the longitude of the second point.
}
  \item{lat2}{
  a scalar giving the latitude of the second point.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A scalar giving the angle (great-circle distance) between the two points in degrees.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
Do not confuse with \code{\link{sl.angle}} which provides the angle between two edges of a triangle.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.gc.dist}}, \code{\link{sl.angle}}.
}
\examples{
sl.lonlat.angle(lon1=0,lat1=50,lon2=10,lat2=40)
## Should return:
## [1] 12.22407
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
