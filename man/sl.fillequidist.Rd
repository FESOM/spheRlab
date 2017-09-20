\name{sl.fillequidist}
\alias{sl.fillequidist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Insert Points into Line on Sphere
}
\description{
Insert points at equidistant locations along a line on a sphere. Note that the algorithm works in x-y-z coordinates. This means that, if more than one point is added, the points are not exactly equidistant in terms of great-circle distances after projection onto the sphere, with deviations becoming large for long lines. (See \code{\link{sl.p2p}} for a more precise alternative where arbitrary fractions can be specified.)
}
\usage{
sl.fillequidist(lon, lat, np = 1)
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
}
\details{
%
}
\value{
\item{lon}{a vector of length \code{np} with the longitude(s) of the inserted points.}
\item{lat}{a vector of length \code{np} with the latitude(s) of the inserted points.}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
