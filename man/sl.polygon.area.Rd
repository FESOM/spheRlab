\name{sl.polygon.area}
\alias{sl.polygon.area}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Polygon Area on Sphere
}
\description{
Compute the area of a polygon on a sphere.
}
\usage{
sl.polygon.area(stamp.lon, stamp.lat, ref.lon = NA, ref.lat = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stamp.lon}{
a vector specifying the longitudes of the polygon vertices.
}
  \item{stamp.lat}{
a vector specifying the latitudes of the polygon vertices.
}
  \item{ref.lon}{
optionally, a scalar specifying the longitude of the reference point from which the polygon is decomposed into triangles. If not specified, the longitude of the barycenter of the vertices will be used.
}
  \item{ref.lat}{
optionally, a scalar specifying the latitude of the reference point from which the polygon is decomposed into triangles. If not specified, the latitude of the barycenter of the vertices will be used.
}
  \item{small}{
a logical value indicating whether or not to use \code{sl.smalltriag.area}. If \code{FALSE} (default), \code{sl.triag.area} will be used, which should be preferred for relatively large polygons (spanning more than a degree or so).
}
  \item{absval}{
a logical value indicating whether or not to return an absolute value (consistent with the usual definition of an 'area'; default). If set to \code{FALSE}, the sign of the result depends on the direction of rotation of the polygon: counterclockwise gives a positive result, and clockwise gives a negative result.
}
}
\details{
First the polygon is decomposed into triangles from a reference point. The latter can be specified by the correspoding arguments; otherwise, the barycenter of the polygon vertices will be used. Second the area of each triangle is computed using \code{sl.triag.area} or \code{sl.smalltriag.area} (depending on the argument \code{small}) and multiplied by the triangle's direction of rotation using \code{sl.checkposition}. Third the results are summed up.
}
\value{
A scalar giving the area of the poygon (on a unit sphere). If \code{absval=TRUE}, the return value is always positive (or zero) as one should expect; otherwise, a positive (negative) return value indicates that the direction of rotation of the polygon is counterclockwise (clockwise).
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
\code{sl.triag.area}, \code{sl.smalltriag.area}
}
\examples{
sl.polygon.area(stamp.lon=c(0,1,1,0),stamp.lat=c(0,0,1,1))
## Should return:
## [1] 0.0003046097
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
