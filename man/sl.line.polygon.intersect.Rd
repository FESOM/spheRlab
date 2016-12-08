\name{sl.line.polygon.intersect}
\alias{sl.line.polygon.intersect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if/where Line Intersects with Polygon
}
\description{
Check if and where a line intersects with a polygon on a sphere.
}
\usage{
sl.line.polygon.intersect(line.lon, line.lat, polygon.lon, polygon.lat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{line.lon}{
  a vector of length 2 giving the longitudes of the points of the first line.
}
  \item{line.lat}{
  a vector of length 2 giving the latitudes of the points of the first line.
}
  \item{polygon.lon}{
  a vector of length \code{N} giving the longitudes of the points defining the polygon.
}
  \item{polygon.lat}{
  a vector of length \code{N} giving the latitudes of the points defining the polygon.
}
}
\details{
This functions is based on \code{sl.line.line.intersect}, which is applied to every line segment of the polygon.
}
\value{
\item{anylines.intersect}{
  a logical value indicating whether the line intersects with the polygon at all.
}
\item{lines.intersect}{
  a vector of length \code{N} with logical values indicating for each segment of the polygon whether the line intersects with the segment (on the shorter ways!).
}
\item{lon,lat,lon2,lat2,x,y,z,x2,y2,z2}{
  vectors of length \code{N} with coordinates for each segment of the polygon where the line intersects with the segment, be it on the shorter or longer ways of the great-circle lines. If the line and the segment intersect on the shorter way (as indicated by \code{lines.intersect}), that point will be in the first set of output variables (that is, the variables without '2' in the name).
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
sl.line.polygon.intersect(line.lon=c(-10,10),line.lat=c(0,5),polygon.lon=c(-5,5,0),polygon.lat=c(0,0,5))
## Should return:
## $anylines.intersect
## [1] TRUE
## 
## $lines.intersect
## [1] FALSE  TRUE  TRUE
## 
## $lon
## [1] 170.000000   1.970366  -3.295047
## 
## $lat
## [1] 0.000000 3.036942 1.710705
## 
## $lon2
## [1]  -10.0000 -178.0296  176.7050
## 
## $lat2
## [1]  0.000000 -3.036942 -1.710705
## 
## $x
## [1] -0.01490451  0.03703484  0.02213033
## 
## $y
## [1]  0.002628067  0.001274107 -0.001274107
## 
## $z
## [1] 0.0000000000 0.0019660214 0.0006620458
## 
## $x2
## [1]  0.01490451 -0.03703484 -0.02213033
## 
## $y2
## [1] -0.002628067 -0.001274107  0.001274107
## 
## $z2
## [1]  0.0000000000 -0.0019660214 -0.0006620458
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
