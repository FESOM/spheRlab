\name{sl.barycenter}
\alias{sl.barycenter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Compute Barycenter of Points
}
\description{
Compute the barycenter of points on a unit sphere. The locations of the points can be provided in x-y-z or lon-lat coordinates. Can be used directly to compute the barycenter (centroid) of a triangle using its vertices, but NOT FOR POLYGONS WITH MORE THAN 3 VERTICES! To that end, use \code{\link{sl.centroid}}.
}
\usage{
sl.barycenter(x = NA, y = NA, z = NA, lon = NA, lat = NA, weights = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a vector of arbitrary length with the x-coordinates of the points on the unit sphere. If specified, \code{lon} and \code{lat} are ignored.
}
  \item{y}{
a vector of the same length as \code{x} with the y-coordinates of the points on the unit sphere. Used only if \code{x} is specified.
}
  \item{z}{
a vector of the same length as \code{x} with the z-coordinates of the points on the unit sphere. Used only if \code{x} is specified.
}
  \item{lon}{
a vector of arbitrary length with the longitudes of the points. Used only if \code{x} is not specified.
}
  \item{lat}{
a vector of the same length as \code{lon} with the latitudes of the points. Used only if \code{x} is not specified.
}
  \item{weights}{
a vector of the length corresponding to the number of points with optional weights of the points.
}
}
\details{
The computation is based on the x-y-z locations of the points. The directly resulting barycenter is located within the unit sphere and projected from the origin back onto the unit sphere.
}
\value{
  \item{lon}{longitude of the barycenter}
  \item{lat}{latitude of the barycenter}
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
\code{\link{sl.centroid}}
}
\examples{
## Using x-y-z as input:
sl.barycenter(x=c(1,0,0),y=c(0,1,0),z=c(0,0,1))

## Using lon-lat as input:
sl.barycenter(lon=c(0,90,0),lat=c(0,0,90))

## Both should return:
## $lon
## [1] 45
## 
## $lat
## [1] 35.26439

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
