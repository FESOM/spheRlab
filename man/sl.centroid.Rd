\name{sl.centroid}
\alias{sl.centroid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Centroid of a Polygon
}
\description{
Compute the centroid (barycenter) of a polygon on a unit sphere. The locations of the vertices need to be provided in lon-lat coordinates.
}
\usage{
sl.centroid(stamp.lon, stamp.lat, ref.lon = NA, ref.lat = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stamp.lon}{
a vector of length >= 3 with the longitudes of the polygon vertices.
}
  \item{stamp.lat}{
a vector of the same length as \code{stamp.lon} with the latitudes of the polygon vertices.
}
  \item{ref.lon}{
Scalar longitude of a point from which to decompose the polygon into triangles. If not specified, the barycenter of the polygon vertices is used.
}
  \item{ref.lat}{
Scalar latitude of a point from which to decompose the polygon into triangles. If not specified, the barycenter of the polygon vertices is used.
}
}
\details{
The polygon is decomposed into triangles formed by any two neighboring vertices and one reference point. If not specified, the barycenter of all polygon vertices is used as reference point. For each triangle the centroid is computed using \code{sl.barycenter} and a weight proportional to the triangle size is assigned. If the direction of rotation around the refrence point is clockwise for a pair of polygon vertices, a negative weight is assigned. The polygon centroid is then computed as the weighted barycenter of the triangle centroids using \code{sl.barycenter}. Note that the overall direction of rotation (the order in which the polygon vertices are provided) does not affect the result because of the internal division by the sum of all weights.
}
\value{
  \item{lon}{Longitude of the centroid}
  \item{lat}{Latitude of the centroid}
  \item{zeroweights}{Number of triangles that were assigned zero weight}
  \item{negweights}{Number of triangles that were assigned a negative weight (due to clockwise rotation).}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
Use this function with caution: it appears that the method is imprecise for large polygons where the curvature of the sphere is not negligible.
}

%% ~Make other sections like Warning with 
%\section{Warning }{...}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
sl.centroid(stamp.lon=c(0,3,4,1.2),stamp.lat=c(-2,1,4,3.5))
## Should return:
## $lon
## [1] 1.905576
## 
## $lat
## [1] 1.592651
## 
## $zeroweights
## [1] 0
## 
## $negweights
## [1] 0
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
