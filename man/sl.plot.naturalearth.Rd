\name{sl.plot.naturalearth}
\alias{sl.plot.naturalearth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Natural Earth Data
}
\description{
Plot Natural Earth data.
}
\usage{
sl.plot.naturalearth(plot.init.res=NULL, load.res=NULL, what="coastline", resolution="medium", poly.split=TRUE, lines.col="black", lwd=1, lty=1, fill.col="grey", fill.refine.boundary=TRUE, fill.refine.boundary.precision=1, polygon.borders=TRUE, polygon.borders.lwd=0.1, polygon.borders.col=fill.col, points.text=TRUE, points.text.col="black", points.text.cex=1, points.text.adj=NULL, points.text.pos=NULL, points.text.offset=0.5, points.text.vfont=NULL, points.text.font=NULL, points.points=FALSE, points.pch=20, points.col="black", points.cex=1, ignore.visibility=FALSE, naturalearth.dir="~/naturalearthdata", verbose=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{\link{sl.plot.init}} (or a variant thereof).
}
  \item{load.res}{
a list as returned by \code{\link{sl.load.naturalearth}}. If NULL (default), \code{\link{sl.load.naturalearth}} is invoked automatically to load the data specified by the arguments 'what' and 'resolution'.}
  \item{what}{
a character or character vector specifying which type of Natural Earth data
to plot, e.g., 'coastline'. To list all possible values for the specified resolution, set what='list'. If 'what' contains more than one element, they are plotted in the order they are provided, that is, the last one will always be in the foreground. Ignored if load.res is not NULL.}
  \item{resolution}{
a character or character vector sepcifying which resolution(s) to use. Possible values are 'coarse' (1:110Mio), 'medium' (1:50Mio; default), and 'fine' (1:10Mio). Ignored if load.res is not NULL.
}
  \item{poly.split}{
a logical value indicating whether polygon objects (e.g. if \code{what='land'}) are to be split into small polygons before plotting. Default is \code{TRUE}. See details in the documentation of the same-named argument in \code{sl.load.naturalearth()}.
}
\item{lines.col}{
a colour or vector of colours to be used for line objects. Default is \code{'black'}.
}
\item{lwd}{
a scalar or numeric vector specifying the line width.
}
\item{lty}{
a line type or vector of line types.
}
\item{fill.col}{
a colour or vector of colours to be used for filling polygons.
}
\item{fill.refine.boundary}{
a logical value or vector of logical values indicating whether or not to refine truncated polygons along curved plot boundaries (in polar projection).
}
\item{fill.refine.boundary.precision}{
a scalar or numeric vector specifying the precision (in degrees) for the refinement of truncated polygons along curved plot boundaries (in polar projection).
}
\item{polygon.borders}{
a logical value or vector of logical values indicating whether or not to plot lines along polygon edges. Default is \code{TRUE}. If splitted polygons are plotted, use this to avoid visible edges between subpolygons.
}
\item{polygon.borders.lwd}{
a scalar or numeric vector specifying the polygon border width. Default is \code{0.1}. If splitted polygons are plotted, use this to avoid visible edges between subpolygons.
}
\item{polygon.borders.col}{
a colour or vector of colours for polygon edges. Default is \code{polygon.borders.col=fill.col} which is useful if splitted polygons are plotted and visible edges between subpolygons shall be avoided.
}
\item{points.text}{
a logical value or vector of logical values indicating whether names associated with point objects shall be plotted.
}
\item{points.text.col}{
a colour or vector of colours to be used for text.
}
\item{points.text.cex}{
a scalar or numeric vector specifying the size scaling factor for text.
}
\item{points.text.adj}{
a numeric vector of length 2 with values in [0, 1] specifying the x and y adjustment of text.
}
\item{points.text.pos}{
a position specifier or vector of position specifiers for the text. If specified this overrides any \code{adj} value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates.
}
\item{points.text.offset}{
a scalar or numeric vector. When \code{points.text.pos} is specified, this value / these values give(s) the offset of the text from the specified coordinate in fractions of a character width.
}
\item{points.text.vfont}{
NULL for the current font family, or a character vector of length 2 for Hershey vector fonts. The first element of the vector selects a typeface and the second element selects a style.
}
\item{points.text.font}{
If \code{vfont = NULL}, the font to be used, possibly a vector. Defaults to the values of the global graphical parameters in \code{par()}.
}
\item{points.points}{
a logical value or vector of logical values indicating whether symbols shall be plotted at the locations of point objects.
}
\item{points.pch}{
a point 'character' (or vector thereof).
}
\item{points.col}{
a point colour (or vector thereof).
}
\item{points.cex}{
a point size scaling factor (or vector thereof).
}
\item{ignore.visibility}{
a logical value indicating whether or not objects shall be attempted to be drawn even if they are invisible, that is, outside the plot domain.
}
  \item{naturalearth.dir}{
a character specifying the local Natural Earth data directory. If data are downloaded from the internet, they will be save to this directory; if it does not yet exist, it will be created. Default is \code{NULL} in which case this is handled automatically in \code{sl.load.naturalearth(), see details there.}
}
  \item{verbose}{
a logical value indicating whether or not to print more information.
}
}
\details{
This function, together with \code{\link{sl.load.naturalearth}}, enables using the openly available and free-to-use Natural Earth data (http://www.naturalearthdata.com) - including coastlines, rivers, land and ocean polygons, and much more - in spheRlab.

If needed, this function invokes \code{\link{sl.load.naturalearth}} to load (and possibly download from the internet) Natural Earth data before plotting. In this case, the package rgdal is needed to read the shape file(s).

If polygon objects are plotted, these are by default (optionally) split into better-behaving small subpolyons before plotting, see documentation of argument \code{poly.split} in \code{\link{sl.load.naturalearth}}.
}
\value{
%%
}
\references{
http://www.naturalearthdata.com
}
\author{
Helge Goessling
}
\note{
%%
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.load.naturalearth}}
}
\examples{
# plot coastlines at coarse resolution:
pir = sl.plot.init()
sl.plot.naturalearth(pir,what="coastline",resolution="coarse")
sl.plot.end(pir)

# list all possible 'what' values for fine resolution:
sl.plot.naturalearth(what="list",resolution="fine")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
