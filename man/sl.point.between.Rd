\name{sl.point.between}
\alias{sl.point.between}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check if Point Between Points
}
\description{
Check if a point is located on the (shortest) line between two points on a sphere, UNDER THE ASSUMPTION THAT the point is located either on the shorter or the longer part of the great-circle line defined by the two points.
}
\usage{
sl.point.between(p.lon, p.lat, line.lon, line.lat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p.lon}{
a scalar specifying the longitude of the point.
}
  \item{p.lat}{
a scalar specifying the latitude of the point.
}
  \item{line.lon}{
a vector of length 2 specifying the longitudes of the line end points.
}
  \item{line.lat}{
a vector of length 2 specifying the latitudes of the line end points.
}
}
\details{
Given that the function works under the assumption that the point is located either on the shorter or the longer part of the great-circle line defined by the two points, the result is not meaningful if that assumption is not met. If in doubt, use \code{sl.checkposition} to check the assumption.
}
\value{
a logical value indicating whether or not the point is located on the shortest line between the two points.
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
\code{sl.checkposition}
}
\examples{
sl.point.between(90,0,c(0,170),c(0,0))
## Should return:
## [1] TRUE

sl.point.between(90,0,c(0,190),c(0,0))
## Should return:
## [1] FALSE
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
