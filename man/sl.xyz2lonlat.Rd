\name{sl.xyz2lonlat}
\alias{sl.xyz2lonlat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert X-Y-Z to Lon-Lat
}
\description{
Covert one or more points from x-y-z (unit sphere) to longitude-latitude coordinates.
}
\usage{
sl.xyz2lonlat(xyzvec=NULL, x=NULL, y=NULL, y=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xyzvec}{
    a vector of length 3 with the x, y, and z coordinates of a single point, or \code{NULL} (default).
  }
  \item{x}{
    a vector or array with the x coordinates of one or more points, or \code{NULL} (default). Used only if \code{xyzvec=NULL}.
  }
  \item{y}{
    a vector or array with the y coordinates of one or more points, or \code{NULL} (default). Used only if \code{xyzvec=NULL}.
  }
  \item{z}{
    a vector or array with the z coordinates of one or more points, or \code{NULL} (default). Used only if \code{xyzvec=NULL}.
  }
}
\details{
If the point(s) is/are not located on the unit sphere, they are accordingly shortened/elongated.
}
\value{
If \code{xyzvec=NULL}, a list with elements \code{lon} and \code{lat}, each of which are vectors or arrays of the same size as \code{x,y,z} with the latitudes and longitudes of the points. Otherwise, a single vector of length 2 with the longitude and latitude of the single point.
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
\code{\link{sl.lonlat2xyz}}
}
\examples{
sl.xyz2lonlat(xyzvec=c(0,1/sqrt(2),1/sqrt(2)))
## Should return:
## [1] 90 45

sl.xyz2lonlat(x=1:5,y=seq(-2,2),z=c(2.543,1.3676,0.432,1.4,-5.22))
## Should return:
## $lon
## [1] -63.43495 -26.56505   0.00000  14.03624  21.80141
## 
## $lat
## [1]  48.674731  31.450344   8.194263  18.754912 -44.107748
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
