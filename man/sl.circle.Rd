\name{sl.circle}
\alias{sl.circle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Points Approximating a Circle on a Sphere
}
\description{
Compute points approximating a circle on a sphere around a central location. Points are placed equidistant on a circle around a central longitude-latitude location with a to-be-specified radius.
}
\usage{
sl.circle(lon, lat, radius, resolution = 1, repeat.first = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a scalar specifying the central longitude.
  }
  \item{lat}{
a scalar specifying the central latitude.
  }
  \item{radius}{
a scalar specifying the radius in degrees.
  }
  \item{resolution}{
a scalar specifying the resolution (distance between points) in degrees (around the central point).
  }
  \item{repeat.first}{
a logical value specifying whether to close the circle explicitly by repeating the first point at the end.
  }
}
\value{
  \item{lon}{Longitudes of the points on the circle}
  \item{lat}{Latitudes of the points on the circle}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}

%% ~Make other sections like Warning with 
%\section{Warning }{...}

\examples{
sl.circle(lon = 8, lat = 50, radius = 5, resolution=30)
# should return:
#$lon
# [1] 15.7507972 15.0886911 12.2782746  8.0000000  3.7217254  0.9113089  0.2492028  1.6076588
# [9]  4.4283457  8.0000000 11.5716543 14.3923412 15.7507972 15.7507972
#
#$lat
# [1] 49.74086 52.29224 54.25711 55.00000 54.25711 52.29224 49.74086 47.31720 45.61144 45.00000
#[11] 45.61144 47.31720 49.74086 49.74086

# Now some nice 'circles' on a lonlat plot:
pir = sl.plot.init(projection="lonlat",do.init.device=FALSE)
res = sl.plot.naturalearth(pir,resolution="coarse")

circ = sl.circle(90,-30,40)
sl.plot.lines(pir,circ$lon,circ$lat,col="green")

circ = sl.circle(-30,70,25)
sl.plot.lines(pir,circ$lon,circ$lat,col="red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
