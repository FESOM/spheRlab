\name{sl.line.lat.intersect}
\alias{sl.line.lat.intersect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if/where Line Intersects with Latitude
}
\description{
Check if and where a (great-circle) line defined by two points on a sphere intersects with a given latitude. Check also if the intersection point(s) lie(s) on the shorther way between the points.
}
\usage{
sl.line.lat.intersect(line.lon, line.lat, lat0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{line.lon}{
  a vector of length 2 with the longitudes of the two points defining the great-circle line.
}
  \item{line.lat}{
  a vector of length 2 with the latitudes of the two points defining the great-circle line.
}
  \item{lat0}{
  a scalar giving the latitude for which intersection points are sought.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{line.lat.intersect}{
  a logical value indicating whether the line intersects with the latitude between the two points (on the shorter way).
}
\item{lat}{
  equals the argument \code{lat0}.
}
\item{lon}{
  if existent, the longitude at which the great circle line (shorter and longer way) intersects with the latitude.
}
\item{line.lat.intersect.twice}{
  a logical value indicating whether the line intersects twice with the latitude between the two points (on the shorter way).
}
\item{lon2}{
  if existent, the second longitude at which the line intersects with the latitude (on the shorter way).
}
\item{gcline.lat.intersect}{
  an integer indicating how often the complete great-circle line (shorter and longer way) intersects with the latitude (must be \code{0}, \code{1}, or \code{2}).
}
\item{x}{
  if existent, a scalar giving the \code{x} coordinate of the first intersection point (on a unit sphere).
}
\item{y}{
  if existent, a scalar giving the \code{y} coordinate of the first intersection point (on a unit sphere).
}
\item{z}{
  if existent, a scalar giving the \code{z} coordinate of the first intersection point (on a unit sphere).
}
\item{x2}{
  if existent, a scalar giving the \code{x} coordinate of the second intersection point (on a unit sphere).
}
\item{y2}{
  if existent, a scalar giving the \code{y} coordinate of the second intersection point (on a unit sphere).
}
\item{z2}{
  if existent, a scalar giving the \code{z} coordinate of the second intersection point (on a unit sphere).
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
sl.line.lat.intersect(line.lon=c(0,1),line.lat=c(0,1),lat0=30)
## Should return:
## $line.lat.intersect
## [1] FALSE
## 
## $lat
## [1] 30
## 
## $lon
## [1] 35.25822
## 
## $line.lat.intersect.twice
## [1] FALSE
## 
## $lon2
## [1] 144.7418
## 
## $gcline.lat.intersect
## [1] 2
## 
## $x
## [1] 0.7071606
## 
## $y
## [1] 0.4999238
## 
## $z
## [1] 0.5
## 
## $x2
## [1] -0.7071606
## 
## $y2
## [1] 0.4999238
## 
## $z2
## [1] 0.5
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
