\name{sl.triag.area}
\alias{sl.triag.area}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Triangle Area on Sphere
}
\description{
Compute the area of a triangle on a sphere.
}
\usage{
sl.triag.area(lon, lat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a vector of length 3 specifying the longitudes of the triangle vertices.
}
  \item{lat}{
a vector of length 3 specifying the longitudes of the triangle vertices.
}
}
\details{
This function is based on Girard's theorem: the area of a triangle on a unit sphere equals the excess of the sum of angles over \code{pi} (180 degrees).
}
\value{
A scalar giving the area of the triangle (on a unit sphere).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
This function is more accurate for large triangles but less accurate for small ones compared to \code{\link{sl.smalltriag.area}}. Also, \code{\link{sl.smalltriag.area}} is computationally cheaper, so for small triangles (<1/100 degrees or so), \code{\link{sl.smalltriag.area}} should be preferred in any case.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.smalltriag.area}}, \code{\link{sl.polygon.area}}
}
\examples{
sl.triag.area(c(0,60,30),c(0,0,30))
## Should return:
## [1] 0.2866951
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
