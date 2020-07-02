\name{sl.plot.points}
\alias{sl.plot.points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Points
}
\description{
Plot points into a spheRlab plot.
}
\usage{
sl.plot.points(plot.init.res, lon, lat, col = "black", pch = 1, ignore.visibility = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{\link{sl.plot.init}} (or a variant thereof).
}
  \item{lon}{
a scalar, vector, or array with \code{N} elements specifying the longitudes of the points.
}
  \item{lat}{
a scalar, vector, or array with \code{N} elements specifying the latitudes of the points.
}
  \item{col}{
a colour (or a vector or array thereof). Can also be used to colour points according to a predefined colour scale, see example.
}
  \item{pch}{
a point 'character' (or a vector or array thereof).
}
  \item{ignore.visibility}{
a logical value indicating whether or not points shall be attempted to be drawn even if they are invisible, that is, outside the plot domain.
}
}
\details{
If \code{lon} and \code{lat} contain no NAs, \code{N} points are drawn (if visible).

This is one of the lowest-level plotting functions of spheRlab.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\code{\link{sl.plot.lines}}, \code{\link{sl.plot.polygon}}
}
\examples{
# initialise spheRlab plot
pir = sl.plot.init(projection = "polar", polar.latbound = 30, do.init.device = FALSE)

# plot some geographical info
sl.plot.lonlatgrid(pir, col = "grey", labels = TRUE, pole.hole = TRUE, labels.col = "darkgrey")
# use the following line (or similar) to plot coastlines (or other geo data); if the corresponding naturalearth
# dataset has not yet been loaded to the computer, this will invoke a download from naturalearthdata.com
# sl.plot.naturalearth(pir, what = "coastline", lines.col = "darkgrey", resolution = "coarse")

# generate evenly distributed random points on the sphere and plot them
Npnts = 1000
pnts = sl.xyz2lonlat(x = rnorm(Npnts), y = rnorm(Npnts), z = rnorm(Npnts))
sl.plot.points(pir, lon = pnts$lon, lat = pnts$lat, pch=4)

# now another random set of points, this time coloured by longitude
pnts2 = sl.xyz2lonlat(x = rnorm(Npnts), y = rnorm(Npnts), z = rnorm(Npnts))
cb = sl.colbar(cols=c("red","blue"),N=12)
col.inds = sl.num2colbar(pnts2$lon,colbar = cb)
# breaks can also be specified manually, which would actually make sense in this example so they are not so odd
sl.plot.points(pir,pnts2$lon,pnts2$lat,col = unlist(cb[col.inds$colour.index]),pch=16)
# the corresponding colourbar can be plotted with sl.plot.
# sl.plot.colbar(cb, breaks = col.inds$breaks, do.init.device = FALSE, labels.num = NULL)

# finish spheRlab plot
sl.plot.end(pir, do.close.device = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
