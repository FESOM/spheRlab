\name{sl.smalltriag.area}
\alias{sl.smalltriag.area}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Small Triangle Area on Sphere
}
\description{
Compute the area of a small triangle on a sphere.
}
\usage{
sl.smalltriag.area(lon, lat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a vector of length 3 specifying the longitudes of the triangle vertices.
}
  \item{lat}{
a vector of length 3 specifying the latitudes of the triangle vertices.
}
}
\details{
This function works in x-y-z coordinates, meaning that it becomes imprecise for larger triangles (e.g. spanning more than a few degrees). However, the function is comparably fast and may therefore be favoured when applied to many small triangles. If accuracy is an issue, use \code{sl.triag.area} instead.
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{sl.triag.area}, \code{sl.polygon.area}
}
\examples{
sl.smalltriag.area(lon=c(0,2,1),lat=c(0,0,1))
## Should return:
## [1] 0.0003045813
## (Compare the result of \code{sl.triag.area}: 0.0003046329)

sl.smalltriag.area(c(0,60,30),c(0,0,30))
## Should return:
## [1] 0.2454951
## (Compare the result of \code{sl.triag.area}: 0.2866951)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
