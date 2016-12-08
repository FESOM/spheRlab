\name{sl.line.line.intersect}
\alias{sl.line.line.intersect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if/where Two Lines Intersect
}
\description{
Check where two lines on a sphere, defined by two times two points, intersect, and whether they intersect on the shorter ways of the great-circle line.
}
\usage{
sl.line.line.intersect(line1.lon, line1.lat, line2.lon, line2.lat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{line1.lon}{
  a vector of length 2 giving the longitudes of the points of the first line.
}
  \item{line1.lat}{
  a vector of length 2 giving the latitudes of the points of the first line.
}
  \item{line2.lon}{
  a vector of length 2 giving the longitudes of the points of the second line.
}
  \item{line2.lat}{
  a vector of length 2 giving the latitudes of the points of the second line.
}
}
\details{
If the lines intersect on the shorter ways of the great-circle line (that is, if \code{lines.intersect=TRUE}), that point will be in the first set of output variables (that is, the variables without '2' in the name). THIS FUNCTION NEEDS MODIFICATION: IT CURRENTLY FAILS WHEN THE LINES ARE IDENTICAL.
}
\value{
\item{lines.intersect}{
  a logical value indicating whether the lines intersect on the shorter ways of the great-circle lines.
}
\item{lon}{
  a scalar giving the longitude of the first intersection point.
}
\item{lat}{
  a scalar giving the latitude of the first intersection point.
}
\item{lon2}{
  a scalar giving the longitude of the second intersection point.
}
\item{lat2}{
  a scalar giving the latitude of the second intersection point.
}
\item{x}{
  a scalar giving the \code{x}-coordinate of the first intersection point.
}
\item{y}{
  a scalar giving the \code{y}-coordinate of the first intersection point.
}
\item{z}{
  a scalar giving the \code{z}-coordinate of the first intersection point.
}
\item{x2}{
  a scalar giving the \code{x}-coordinate of the second intersection point.
}
\item{y2}{
  a scalar giving the \code{y}-coordinate of the second intersection point.
}
\item{y3}{
  a scalar giving the \code{z}-coordinate of the second intersection point.
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
sl.line.line.intersect(line1.lon=c(0,10),line1.lat=c(0,0),line2.lon=c(0,10),line2.lat=c(10,5))
## Should return:
## $lines.intersect
## [1] FALSE
## 
## $lon
## [1] -160.4361
## 
## $lat
## [1] 0
## 
## $lon2
## [1] 19.56389
## 
## $lat2
## [1] 0
## 
## $x
## [1] -0.01467808
## 
## $y
## [1] -0.005216208
## 
## $z
## [1] 0
## 
## $x2
## [1] 0.01467808
## 
## $y2
## [1] 0.005216208
## 
## $z2
## [1] 0
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
