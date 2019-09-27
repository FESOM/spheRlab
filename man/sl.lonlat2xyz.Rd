\name{sl.lonlat2xyz}
\alias{sl.lonlat2xyz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Lon-Lat to X-Y-Z
}
\description{
Covert one or more points from longitude-latitude to x-y-z coordinates on a unit sphere.
}
\usage{
sl.lonlat2xyz(lonlatvec=NULL, lon=NULL, lat=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lonlatvec}{
    a vector of length 2 with the longitude and latitude of a single point, or \code{NULL} (default).
  }
  \item{lon}{
    a vector or array with the longitudes of one or more points, or \code{NULL} (default). Used only if \code{xyzvec=NULL}.
  }
  \item{lat}{
    a vector or array with the latitudes of one or more points, or \code{NULL} (default). Used only if \code{xyzvec=NULL}.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
If \code{lonlatvec=NULL}, a list with elements \code{x}, \code{y}, and \code{z}, each of which are vectors or arrays of the same size as \code{lon,lat} with the x-y-z coordinates of the points on the unit sphere. Otherwise, a single vector of length 3 with the x-y-z coordinates of the single point on the unit sphere.
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
\code{\link{sl.xyz2lonlat}}
}
\examples{
sl.lonlat2xyz(lonlatvec=c(10,20))
## Should return:
## [1] 0.9254166 0.1631759 0.3420201

sl.lonlat2xyz(lon=seq(-180,180,60), lat=seq(-90,90,30))
## Should return:
## $x
## [1] -6.123234e-17 -2.500000e-01  4.330127e-01  1.000000e+00  4.330127e-01 -2.500000e-01 -6.123234e-17
## 
## $y
## [1] -7.498799e-33 -4.330127e-01 -7.500000e-01  0.000000e+00  7.500000e-01  4.330127e-01  7.498799e-33
## 
## $z
## [1] -1.0000000 -0.8660254 -0.5000000  0.0000000  0.5000000  0.8660254  1.0000000
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
