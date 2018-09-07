\name{sl.plot.fld.curvilin}
\alias{sl.plot.fld.curvilin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Curvilinear Field
}
\description{
Plot a 2-dimensional numerical field on a curvilinear grid.
}
\usage{
sl.plot.fld.curvilin(plot.init.res, vals, mask = NULL, lon.i, lat.i, border = TRUE, border.lwd = 0.01, colbar = sl.colbar.blackwhite_256, breaks = NA, breaks.log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{\link{sl.plot.init}} (or a variant thereof).
}
  \item{vals}{
an \code{Nlat}x\code{Nlon} matrix specifying a 2-dimensional numerical curvilinear field.
}
  \item{mask}{
an \code{Nlat}x\code{Nlon} matrix with logical values specifying which quadrangles to draw. Default is \code{mask=NULL}, equivalent to all matrix elements being \code{TRUE} meaning that all quadrangles are drawn.
}
  \item{lon.i}{
an \code{Nlat+1}x\code{Nlon+1} matrix specifying the interface longitudes corresponding to \code{vals}.
}
  \item{lat.i}{
an \code{Nlat+1}x\code{Nlon+1} matrix specifying the interface latitudes corresponding to \code{vals}.
}
  \item{border}{
a logical value indicating whether or not to draw lines for the polygon borders.
}
  \item{border.lwd}{
the line width of the borders.
}
  \item{colbar}{
a spheRlab colourbar (as returned by \code{\link{sl.colbar}}) of length \code{Nc} to be used for the field.
}
  \item{colbar.breaks}{
a numeric vector of length \code{Nc-1} specifying the breaks between colours.
}
  \item{colbar.breaks.log}{
a logical value indicating whether colourbar breaks are to be placed logarithmically.
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
\code{\link{sl.plot.field}}, \code{\link{sl.colbar}}, \code{\link{sl.num2colbar}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
