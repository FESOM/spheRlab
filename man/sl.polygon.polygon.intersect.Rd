\name{sl.polygon.polygon.intersect}
\alias{sl.polygon.polygon.intersect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if/where Polygons Intersect
}
\description{
UNFINISHED ### Check if and where two polygons on a sphere intersect each other. Technically, the intersection of each pair of polygon edges is checked.
}
\usage{
sl.polygon.polygon.intersect(poly1.lon, poly1.lat, poly2.lon, poly2.lat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{poly1.lon}{
  a vector of length \code{N1} giving the longitudes of the points defining the first polygon.
}
  \item{poly1.lat}{
  a vector of length \code{N1} giving the latitudes of the points defining the first polygon.
}
  \item{poly2.lon}{
  a vector of length \code{N2} giving the longitudes of the points defining the second polygon.
}
  \item{poly2.lat}{
  a vector of length \code{N2} giving the latitudes of the points defining the second polygon.
}
}
\details{
To be provided once finished ...
}
\value{
To be provided once finished ...
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
\code{\link{sl.line.polygon.intersect}}, \code{\link{sl.line.line.intersect}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
