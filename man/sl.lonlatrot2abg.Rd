\name{sl.lonlatrot2abg}
\alias{sl.lonlatrot2abg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Rotation Lon-Lat-Rot to Alpha-Beta-Gamma
}
\description{
Convert rotation parameters from longitude-latitude-rotation to alpha-beta-gamma.
}
\usage{
sl.lonlatrot2abg(lonlatrot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  A vector of length 3 with the following elements:
\item{lonlatrot[1]}{
  The longitude of the point that is supposed to be the new North Pole.
}
\item{lonlatrot[2]}{
  The latitude of the point that is supposed to be the new North Pole.
}
\item{lonlatrot[3]}{
  The angle (in degrees) of rotation around the new North Pole.
}
}
\details{
%
}
\value{
A vector of length 3 with the parameters alpha, beta, and gamma as used in \code{\link{sl.rot}}. Note that to achieve identity transformation (that is, no transformation at all) the third element of the argument must be \code{-90} (see the second example).
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
\code{\link{sl.rot}}
}
\examples{
abg = sl.lonlatrot2abg(lonlatrot=c(30,0,5))
sl.rot(lon=0,lat=0,alpha=abg[1],beta=abg[2],gamma=abg[3])
## Should return: 
## $lon
## [1] 175
## 
## $lat
## [1] 60
sl.rot(lon=30,lat=0,alpha=abg[1],beta=abg[2],gamma=abg[3])
## Should return: $lon
## [1] -20.55605
## 
## $lat
## [1] 90

abg = sl.lonlatrot2abg(lonlatrot=c(0,90,-90))
sl.rot(lon=-150,lat=40,alpha=abg[1],beta=abg[2],gamma=abg[3])
## Should return:
## $lon
## [1] -150
## 
## $lat
## [1] 40
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
