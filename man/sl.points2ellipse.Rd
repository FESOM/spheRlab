\name{sl.points2ellipse}
\alias{sl.points2ellipse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw Ellipse around Points on a Sphere
}
\description{
Draw an ellipse representing the cumulative distribution function of a bivariate normal distribution fitted to points on a sphere based on the covariance matrix.
}
\usage{
sl.points2ellipse <- function (lon = NULL, lat = NULL, prob.within = 0.95, npoints = 360, return.ellispecs = F, Rsphere = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a numeric vector specifying the longitudes of the points.
}
  \item{lat}{
a numeric vector specifying the latitudes of the points.
}
  \item{prob.within}{
a numeric scalar specifying the integrated probability to be enclosed by the ellipse.
}
  \item{npoints}{
an integer scalar specifying the number of points comprising the border of the ellipse.
}
  \item{return.ellispecs}{
a logical value specifying if area and semi-major and -minor axis of the resulting ellipse should be returned.
}
  \item{Rsphere}{
a numeric scalar defining the radius of the sphere, needed for calculating the area of the ellipse, default is \code{Rsphere = 1}. The units for the semi-axes will correspond to the units of \code{Rsphere}.
}
}
\details{
The points are rotated so that their barycenter is the new pole. Then, the function \code{ellipse} from the package \code{mixtools} is used to derive the ellipse on the xy-plane. Finally, the points are projected back onto the sphere with x,y fixed.

The latter implies that a very large ellipse obtained due to a large \code{prob.within} or due to excessive spread of the point cloud (e.g., spanning more than one hemisphere) will cause an error. However, in that case the assumptions underlying in the projection from the sphere to the xy-plane and back are violated too heavily anyway.

Note that the area of the ellipse is calculated in the projection on the xy-plane, i.e. the curvature is ignored.
}

\value{
A list with the following elements:
  \item{lon}{a numeric vector giving the longitudes of the points describing the ellipse}
  \item{lat}{a numeric vector giving the longitudes of the points describing the ellipse}
  \item{area}{a numeric scalar giving the area of the ellipse - only if \code{return.ellispecs = TRUE}}
  \item{a,b}{numeric scalars for semi-major and semi-minor axis length - only if \code{return.ellispecs = TRUE}}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling, modified by Simon Reifenberg
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{mixtools::ellipse}
}
\examples{
lon0 = rnorm(100,sd=15)
lat0 = rnorm(100,sd=5)
angle = pi/6
lon = lon0 * cos(angle) - lat0 * sin(angle)
lat = lon0 * sin(angle) + lat0 * cos(angle)

res = sl.points2ellipse(lon=lon,lat=lat,prob.within=0.9)

plot(lon,lat)
lines(res$lon,res$lat,col="red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
