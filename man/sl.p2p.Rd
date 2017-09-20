\name{sl.p2p}
\alias{sl.p2p}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Point Between Points on Sphere
}
\description{
Compute a point on a great-circle line moving a certain fraction from a start point towards an end point.
}
\usage{
sl.p2p(lon1, lat1, lon2, lat2, frac)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon1}{
a scalar giving the longitude of the start point.
}
  \item{lat1}{
a scalar giving the latitude of the start point.
}
  \item{lon2}{
a scalar giving the longitude of the end point.
}
  \item{lat2}{
a scalar giving the latitude of the start point.
}
  \item{frac}{
a scalar giving the fraction how much of the distance from the start point to the end point shall be covered. Values below \code{0} or above \code{1} are valid and result in points outside the connecting line between the start and end points.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{lon}{
	a scalar giving the longitude of the resulting point.
}
\item{lat}{
	a scalar giving the latitude of the resulting point.
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
\code{\link{sl.fillequidist}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
sl.p2p(lon1=0,lat1=0,lon2=90,lat2=40,frac=0.5)
## Should return:
## $lon
## [1] 37.45372
## 
## $lat
## [1] 27.03402
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
