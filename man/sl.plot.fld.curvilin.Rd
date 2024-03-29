\name{sl.plot.fld.curvilin}
\alias{sl.plot.fld.curvilin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Curvilinear Field
}
\description{
Plot a 2-dimensional numerical or RGB(Alpha)-coded field on a curvilinear grid.
}
\usage{
sl.plot.fld.curvilin(plot.init.res, vals = NULL, rgba = NULL, mask = NULL, lon.i, lat.i, border = TRUE, border.lwd = 0.01, colbar = sl.colbar.blackwhite_256, breaks = NA, breaks.log = FALSE, na.col = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{\link{sl.plot.init}} (or a variant thereof).
}
  \item{vals}{
an \code{Nlat}x\code{Nlon} matrix specifying a 2-dimensional numerical curvilinear field, or \code{NULL} (default), in which case \code{rgba} must be provided.
}
  \item{rgba}{
a list with the elements \code{r}, \code{g}, and \code{b} (and optionally \code{a}), each of which is an \code{Nlat}x\code{Nlon} matrix specifying a 2-dimensional RGB(Alpha)-coded curvilinear field, or \code{NULL} (default), in which case \code{vals} must be provided.
}
  \item{mask}{
an \code{Nlat}x\code{Nlon} matrix with logical values specifying which quadrangles to draw. Default is \code{mask=NULL}, equivalent to all matrix elements being \code{TRUE} meaning that all quadrangles (except NA values if \code{na.col=NULL}) are drawn.
}
  \item{lon.i}{
an \code{Nlat+1}x\code{Nlon+1} matrix specifying the interface longitudes corresponding to \code{vals}.
}
  \item{lat.i}{
an \code{Nlat+1}x\code{Nlon+1} matrix specifying the interface latitudes corresponding to \code{vals}.
}
  \item{border}{
a logical value indicating whether or not to draw lines for the polygon borders. Default is \code{TRUE}, which can avoid aliasing that may occur when plots are rendered. If aliasing still occurs, you can try to increase \code{border.lwd}.
}
  \item{border.lwd}{
the line width of the borders. Default is \code{1}. Used only if \code{border=TRUE}. If this is used to avoid aliasing when plots are rendered, but aliasing still occurs, you can try to increase this value.
}
  \item{colbar}{
a spheRlab colourbar (as returned by \code{\link{sl.colbar}}) of length \code{Nc} to be used for the numerical field. Used only if \code{vals} is specified.
}
  \item{colbar.breaks}{
a numeric vector of length \code{Nc-1} specifying the breaks between colours. Used only if \code{vals} is specified.
}
  \item{colbar.breaks.log}{
a logical value indicating whether colourbar breaks are to be placed logarithmically. Used only if \code{vals} is specified.
}
  \item{na.col}{
the color to be used for NA values. Default is \code{na.col=NULL}, meaning that no polygons are drawn where \code{vals=NA}.
}
}
\details{
For each point of the curvilinear field, a quadrangle as defined by the interface longitudes and latitudes is drawn, with colouring according to the numerical value of the point and the colourbar provided.
}
\value{
The colourbar will be returned for further usage (e.g., for plotting of the colourbar).
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
\code{\link{sl.plot.field}}, \code{\link{sl.colbar}}, \code{\link{sl.num2colbar}}, \code{\link{sl.lonlat2D.c2i}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
