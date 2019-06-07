\name{sl.gc.dist}
\alias{sl.gc.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Great-Circle Distance
}
\description{
Compute great-circle distances between two or more points on a sphere. If coordinates for more than two points are provided, either distances between consecutive points, between the first and all other points, or between independent pairs of points are computed.
}
\usage{
sl.gc.dist(lon, lat, Rsphere = 1, sequential = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a vector of length \code{N} >= 2 or an \code{Mx2} matrix or data frame with the longitudes of the points.
}
  \item{lat}{
a vector of length \code{N} >= 2 or an \code{Mx2} matrix or data frame with the latitudes of the points.
}
  \item{Rsphere}{
a scalar value giving the radius of the sphere. Default is a unit sphere, that is, \code{Rsphere=1}.
}
  \item{sequential}{
a logical value specifying for the case that \code{lon} and \code{lat} are vectors and \code{N} > 2 whether distances are computed between consecutive pairs of points (default). If \code{sequential=FALSE}, distances from the first point to all other points are computed.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of length \code{N-1} or \code{M} (i.e., a scalar if \code{N=2} or \code{M=1}) with the great-circle distances between the points.
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
\code{\link{sl.cart.dist}}
}
\examples{
sl.gc.dist(lon=c(0,30,90,0),lat=c(0,0,0,90))
## Should return:
## [1] 0.5235988 1.0471976 1.5707963

sl.gc.dist(lon=c(0,30,90,0),lat=c(0,0,0,90),sequential=FALSE)
## Should return:
## [1] 0.5235988 1.5707963 1.5707963

sl.gc.dist(lon=data.frame(lon1=c(10,25,30),lon2=c(20,35,0)),lat=data.frame(lat1=c(0,0,10),lon2=c(20,35,50)))
## Should return:
## [1] 0.3886629 0.6322375 0.8213473
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
