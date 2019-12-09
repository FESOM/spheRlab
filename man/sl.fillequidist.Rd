\name{sl.fillequidist}
\alias{sl.fillequidist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Insert Points into Line on Sphere
}
\description{
Insert points at equidistant locations along a line on a sphere. By default (for backwards compatibility) the algorithm works in x-y-z coordinates. This means that, if more than one point is added, the points are not exactly equidistant in terms of great-circle distances after projection onto the sphere, with deviations becoming large for long lines. To obtain accurate equidistant spacing on the sphere, set \code{method="gc"}.

See \code{\link{sl.p2p}} for an alternative where arbitrary fractions can be specified.
}
\usage{
sl.fillequidist(lon, lat, np = 1, method = "xyz", return.xyz = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a vector of length 2 with the longitudes of the start and end points of the line.
}
  \item{lat}{
a vector of length 2 with the latitudes of the start and end points of the line.
}
  \item{np}{
an integer giving the number of points to be inserted. Default is 1 (in which case the algorithm is exact even for long lines).
}
  \item{method}{
a character specifying whether points are placed equidistant in x-y-z space and then reprojected onto the sphere (\code{method="xyz"}, default), or equidistant in terms of resulting great-circle distance (\code{method="gc"}).
}
  \item{return.xyz}{
a logical value specifying whether to return also x-y-z coordinates of the resulting points. Default is \code{FALSE}.
}
}
\details{
%
}
\value{
A list with the following elements, with the last three being optional, depending on \code{return.xyz}:
\item{lon}{a vector of length \code{np} with the longitude(s) of the inserted point(s).}
\item{lat}{a vector of length \code{np} with the latitude(s) of the inserted point(s).}
\item{x}{a vector of length \code{np} with the x-coordinate(s) of the inserted point(s).}
\item{y}{a vector of length \code{np} with the y-coordinate(s) of the inserted point(s).}
\item{z}{a vector of length \code{np} with the z-coordinate(s) of the inserted point(s).}
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
\code{\link{sl.p2p}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
sl.fillequidist(c(0,10),c(0,0),np=2)
## Should return:
## $lon
## [1] 3.329563 6.670437
## 
## $lat
## [1] 0 0

sl.fillequidist(c(0,10),c(0,0),np=2,method="gc")
## Should return:
## $lon
## [1] 3.333333 6.666667
## 
## $lat
## [1] 2.345519e-15 1.174747e-15
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
