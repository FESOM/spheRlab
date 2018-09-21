\name{sl.boundingcircle}
\alias{sl.boundingcircle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find a (Small) Bounding Circle For a Set of Points on a Sphere
}
\description{
Find a bounding circle for a set of points on a sphere. Ideally, this would be the smallest possible of such circles, but currently only a fast approximate method ('Ritter') is implemented which provides a bounding circle that is typically 5-20\% larger than the smallest possible.
}
\usage{
sl.boundingcircle(lon=NULL, lat=NULL, method="ritter", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a numeric vector specifying the longitudes of the points to be enclosed.
  }
  \item{lat}{
a numeric vector specifying the latitudes of the points to be enclosed.
  }
  \item{method}{
a character specifying the method to be used; currently only 'ritter' is implemented which is fast but provides a bounding circle that is typically 5-20\% larger than the smallest possible.
  }
  \item{verbose}{
a logical value specifying whether or not to warn about the approximate 'Ritter' method.
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
lons = rnorm(10)*20 + 30
lats = rnorm(10)*20 - 20
bounding.circ = sl.boundingcircle(lon = lons, lat = lats)

pir = sl.plot.init(projection="lonlat",do.init.device=FALSE)
res = sl.plot.naturalearth(pir,resolution="coarse")
sl.plot.points(pir,lons,lats,col="blue")
circ = sl.circle(lon = bounding.circ$center_lon, lat = bounding.circ$center_lat, radius = bounding.circ$radius)
sl.plot.lines(pir,circ$lon,circ$lat,col="red")
sl.plot.end(pir,do.close.device=FALSE)

# Can also be used to determine automatically which part of the map to plot in 'polar' projection, for example (re-using the points and their bounding circle used above):
pir = sl.plot.init(projection="polar",polar.lonlatrot=c(bounding.circ$center_lon,bounding.circ$center_lat,0),polar.latbound=(90-1.1*(bounding.circ$radius)),do.init.device=FALSE)
res = sl.plot.naturalearth(pir,resolution="coarse")
res = sl.plot.lonlatgrid(pir,labels=TRUE)
sl.plot.points(pir,lons,lats,col="blue")
sl.plot.end(pir,do.close.device=FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
