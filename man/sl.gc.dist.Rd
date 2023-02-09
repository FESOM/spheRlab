\name{sl.gc.dist}
\alias{sl.gc.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Great-Circle Distance
}
\description{
Compute great-circle distances between two or more points on a sphere. If coordinates for more than two points are provided, either distances between consecutive points, between the first and all other points, between specific pairs of points, or between all pairs of points are computed.
}
\usage{
sl.gc.dist(lon = NULL, lat = NULL, x = NULL, y = NULL, z = NULL, Rsphere = 1, sequential = TRUE, distmat = FALSE, byrot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a vector of length \code{N} >= 2 or an \code{Mx2} matrix or data frame with the longitudes of the points. Not required if \code{(x,y,z)} are provided.
}
  \item{lat}{
a vector of length \code{N} >= 2 or an \code{Mx2} matrix or data frame with the latitudes of the points. Not required if \code{(x,y,z)} are provided.
}
  \item{x}{
a vector of length \code{N} >= 2 or an \code{Mx2} matrix or data frame with the x-coordinates of the points. Not required if \code{(lon,lat)} are provided. Note that \code{sqrt(x^2+y^2+z^2)=1} must hold, that is, \code{(x,y,z)} must be located on a unit sphere (inpedendent of \code{Rsphere} which only affects the values of the returned distances).
}
  \item{y}{
a vector of length \code{N} >= 2 or an \code{Mx2} matrix or data frame with the y-coordinates of the points. Not required if \code{(lon,lat)} are provided. Note that \code{sqrt(x^2+y^2+z^2)=1} must hold, that is, \code{(x,y,z)} must be located on a unit sphere (inpedendent of \code{Rsphere} which only affects the returned distance).
}
  \item{z}{
a vector of length \code{N} >= 2 or an \code{Mx2} matrix or data frame with the z-coordinates of the points. Not required if \code{(lon,lat)} are provided. Note that \code{sqrt(x^2+y^2+z^2)=1} must hold, that is, \code{(x,y,z)} must be located on a unit sphere (inpedendent of \code{Rsphere} which only affects the returned distance).
}
  \item{Rsphere}{
a scalar value giving the radius of the sphere. Default is a unit sphere, that is, \code{Rsphere=1}.
}
  \item{sequential}{
a logical value specifying for the case that \code{lon,lat} or \code{x,y,z} are vectors and \code{N} > 2 whether distances are computed between consecutive pairs of points (default). If \code{sequential=FALSE}, distances from the first point to all other points are computed.
}
  \item{distmat}{
a logical value specifying whether distances are computed between all pairs of points. In this case, the built-in function \code{\link{dist}} is used (followed by a conversion to great-circle distance) and an \code{NxN} matrix is returned. Default is \code{FALSE}.
}
  \item{byrot}{
a logical value specifying whether distances are computed by rotation, which can be slow. Default is \code{FALSE}, meaning that distances are computed by the equation \code{(2*Rsphere)*asin(sqrt(sum((xyz_2 - xyz_1)^2))/2)}.
}
}
\details{
As detailed in the function argument description above, this function can handle different types of inputs (\code{lon,lat} versus \code{x,y,z}; vectors versus 2-column matrices), use different approaches for the computation (rotation versus cartesian distance versus built-in \code{dist} function), and compute distances between different sets of points (first against all others; subsequent pairs; all pairs).
}
\value{
Either a vector of length \code{N-1} or \code{M} (i.e., a scalar if \code{N=2} or \code{M=1}) with the great-circle distances between the points, or an \code{MxM} matrix with all pairwise distances.
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

sl.gc.dist(lon=c(0,30,90,0),lat=c(0,0,0,90),distmat=TRUE)
## Should return:
##           1         2        3        4
## 1 0.0000000 0.5235988 1.570796 1.570796
## 2 0.5235988 0.0000000 1.047198 1.570796
## 3 1.5707963 1.0471976 0.000000 1.570796
## 4 1.5707963 1.5707963 1.570796 0.000000
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
