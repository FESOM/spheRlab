\name{sl.inpolygon.convex}
\alias{sl.inpolygon.convex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if Point Inside Convex Polygon
}
\description{
Check whether a point is located inside a convex polygon on a sphere.
}
\usage{
sl.inpolygon.convex(lon.point, lat.point, lon, lat, check.ccw.convex = TRUE, border.as.in = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon.point}{
  a scalar giving the longitude of the point.
}
  \item{lat.point}{
  a scalar giving the latitude of the point.
}
  \item{lon}{
  a vector giving the longitudes of the polygon.
}
  \item{lat}{
  a vector giving the latitudes of the polygon.
}
  \item{check.ccw.convex}{
  a logical value indicating whether it is checked at the beginning if the polygon fulfills the requirements for the algorithm to work, that is, if the polygon is ordered counterclockwise and if it is convex. The default is \code{check.ccw.convex=TRUE}. If the requirements are known to be true, setting \code{check.ccw.convex=FALSE} increases the computational efficiency.
}
  \item{border.as.in}{
  a logical value indicating whether the polygon edges are counted as 'inside'.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A logical value indicating whether the point is located inside the polygon.
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
sl.inpolygon.convex(lon.point=0,lat.point=0,lon=c(-10,-10,10,10),lat=c(10,-10,-10,10))
## Should return:
## [1] TRUE

sl.inpolygon.convex(lon.point=0,lat.point=20,lon=c(-10,-10,10,10),lat=c(10,-10,-10,10))
## Should return:
## [1] FALSE
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
