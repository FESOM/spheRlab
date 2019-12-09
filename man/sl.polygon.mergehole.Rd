\name{sl.polygon.mergehole}
\alias{sl.polygon.mergehole}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Merge Hole in Polygon
}
\description{
Merge a hole ('inverse') polygon with a larger polygon that encompasses the hole, on a sphere, returning either a single merged polygon (with duplicate points at a single connecting segment) or two polygons where a second connecting segment is used to obtain two polygons, each of which individually being free of duplicate points. Additional arguments allow some control over the way how the connecting segments are constructed.
}
\usage{
sl.polygon.mergehole(poly, poly.hole, connect.maxstep=Inf, checkcross=TRUE, split.poly=FALSE, split.costexp=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{poly}{
a list with two elements \code{lon} and \code{lat}, which are numeric vectors of length \code{N1} specifying the longitudes and latitudes of the outer polygon.
}
  \item{poly.hole}{
a list with two elements \code{lon} and \code{lat}, which are numeric vectors of length \code{N2} specifying the longitudes and latitudes of the inner polygon.
}
  \item{connect.maxstep}{
either a numeric scalar (in radians) specifying an upper bound for the step length used to subdivide the connecting segment(s), or a function that determines this upper bound based on the length of all segments comprising \code{poly} and \code{poly.hole} (e.g. \code{max}). Default is \code{connect.maxstep=Inf}, in which case no points are added. See details below.
}
  \item{checkcross}{
a logical value specifying whether or not (i) to check the two input polygons for integrity, meaning that no intersections between the segments comprising each polygon are present, and (ii) to make sure that the connection(s) do not intesect with any polygon segment. Default is \code{TRUE}.
}
  \item{split.poly}{
  a logical value specifying whether two polygons (using a second connecting segment), both individually being free of duplicate points, shall be returned. If \code{FALSE} (default), a single merged polygon with duplicate points at a single connecting segment is returned.
}
  \item{split.costexp}{
  a numeric scalar specifying a parameter in the cost function used to determine the location of the second connection, used only if \code{split.poly=TRUE}, see details below. Default is \code{split.costexp=2}.
}
}
\details{
The first connection between the outer and inner polygons is determined simply by minimizing the distances between the connected points. If \code{checkcross=TRUE} and the connection over the shortest distance would cross other polygon segments, those two points with the second-shortest distance are tried, and so forth. The first connection is determined independently of the optional second connection to split the polygon in two pieces.

The cost function used to determine the second connection (if \code{split.poly=TRUE}) favours a small distance between the connected points, too, but now it also favours a long minimum distance along the polygon boundaries between the first and second connections (minimum over the four polygon boundary pieces, two inner and two outer). The cost function to be minimized reads \code{COST = length_second_connection / min(inter_connection_distance_along_polygon_boundary)^split.costexp}. A larger value for \code{split.costexp} thus means that a large (minimum) along-boundary distance is considered relatively more important compared to a short connection distance, leading tendentially to more equally-sized polygons.

If a connection is longer than \code{connect.maxstep}, the minimum number of points required to obtain sub-segments shorter than (or as long as) the upper bound are added in an equidistant manner along the connection using \code{sl.fillequidist(...,method="gc")}. For example, \code{connect.maxstep=max} will make sure that individual segments of the connection(s) will not be longer than the longest segment of the two input polygons. This can be useful if the resulting polygons are to be split further, e.g., with \code{\link{sl.polygon.split}}.
}
\value{
If \code{split.poly=FALSE}, a list with the following two elements is returned (where \code{C1} is the number of points inserted into the connecting segment):
\item{lon}{a vector of length \code{N1+N2+2*C1} with the longitude(s) of the merged polygon.}
\item{lat}{a vector of length \code{N1+N2+2*C1} with the latitude(s) of the merged polygon.}

If \code{split.poly=TRUE}, a list with the following two elements is returned (where \code{N3+N4 = N1+N2+2*(C1+C2)} and \code{C1} and \code{C2} are the numbers of points inserted into the first and second connecting segments):
\item{polygon1}{a list with elements \code{lon} and \code{lat}, each a vector of length \code{N3}, that provide the longitudes and latitudes of the first polygon.}
\item{polygon2}{a list with elements \code{lon} and \code{lat}, each a vector of length \code{N4}, that provide the longitudes and latitudes of the second polygon.}
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
\code{\link{sl.finddist}}, \code{\link{sl.intersect}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
lon1 = c(5,0,9,0.5,0,10,10)
lat1 = c(10,9.8,9.6,9.2,0,0,9.9)
lon2 = c(2,5,8)
lat2 = c(3,9,4)

res.nosplit = sl.polygon.mergehole(poly=list(lon=lon1,lat=lat1), poly.hole=list(lon=lon2,lat=lat2), split.poly=FALSE)
str(res.nosplit)
## Should return:
## List of 2
##  $ lon: num [1:12] 0 10 10 5 0 9 0.5 0 2 5 ...
##  $ lat: num [1:12] 0 0 9.9 10 9.8 9.6 9.2 0 3 9 ...

res.split = sl.polygon.mergehole(poly=list(lon=lon1,lat=lat1), poly.hole=list(lon=lon2,lat=lat2), split.poly=TRUE, connect.maxstep=function(x){min(x)/2})
str(res.split)
## Should return:
## List of 2
##  $ polygon1:List of 2
##   ..$ lon: num [1:6] 0 10 9 8 2 ...
##   ..$ lat: num [1:6] 0 0 2 4 3 ...
##  $ polygon2:List of 2
##   ..$ lon: num [1:12] 10 10 5 0 9 ...
##   ..$ lat: num [1:12] 0 9.9 10 9.8 9.6 ...

plot(NA, xlim=range(c(lon1,lon2)), ylim=range(c(lat1,lat2)))
polygon(res.split$polygon1$lon, res.split$polygon1$lat, col="red", border=NA)
polygon(res.split$polygon2$lon, res.split$polygon2$lat, col="blue", border=NA)
polygon(lon1, lat1, lwd=2)
polygon(lon2, lat2, lwd=2, lty=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
