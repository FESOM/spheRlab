\name{sl.plot.text}
\alias{sl.plot.text}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Text
}
\description{
Plot simple text into a spheRlab plot
}
\usage{
sl.plot.text(plot.init.res, lon, lat, labels = seq(1, length(lon)), col = "black", ignore.visibility = FALSE, adj = NULL, pos = NULL, offset = 0.5, vfont = NULL, cex = 1, font = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{sl.plot.init} (or a variant thereof).
}
  \item{lon}{
a scalar or vector specifying the longitude(s) of the text location(s).
}
  \item{lat}{
a scalar or vector specifying the latitude(s) of the text location(s).
}
  \item{labels}{
a character or character vector specifying the text(s).
}
  \item{col}{
the color to be used, possibly a vector. Default is \code{col="black"}.
}
  \item{ignore.visibility}{
a logical value indicating whether or not to ignore visibility.
}
  \item{adj}{
one or two values in [0, 1] which specify the x (and optionally y) adjustment of the labels.
}
  \item{pos}{
a position specifier for the text. If specified this overrides any \code{adj} value given. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the specified coordinates.
}
  \item{offset}{
when \code{pos} is specified, this value gives the offset of the label from the specified coordinate in fractions of a character width.
}
  \item{vfont}{
\code{NULL} for the current font family, or a character vector of length 2 for Hershey vector fonts. The first element of the vector selects a typeface and the second element selects a style. Ignored if labels is an expression.
}
  \item{cex}{
numeric character expansion factor; multiplied by \code{par("cex")} yields the final character size. \code{NULL} and \code{NA} are equivalent to \code{1.0}.
}
  \item{font}{
If \code{vfont=NULL}, the font to be used, possibly a vector. Defaults to the value of the global graphical parameter in \code{par()}.
}
}
\details{
Plots simple text at lon-lat locations, using the \code{R} function \code{text}. Numerous arguments are simply passed to \code{text}.
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
It would be nice if, at some point, a function would be added to spheRlab that allows plotting text on the spherical surface, probably just making use of \code{sl.plot.polygon}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (plot.init.res, lon, lat, labels = seq(1, length(lon)), 
    col = "black", ignore.visibility = FALSE, adj = NULL, pos = NULL, 
    offset = 0.5, vfont = NULL, cex = 1, font = NULL) 
{
    projection = plot.init.res$projection
    if (projection == "platon" || projection == "3D") {
        npir = 1
        repeat {
            pir = plot.init.res[[npir]]
            if (!is.list(pir)) {
                return()
            }
            if (is.null(pir$projection)) {
                return()
            }
            sl.plot.text(pir, lon, lat, labels, col, ignore.visibility, 
                adj, pos, offset, vfont, cex, font)
            npir = npir + 1
        }
    }
    vsr.res = sl.vis.shift.rot(plot.init.res, lon, lat)
    x = vsr.res$x
    y = vsr.res$y
    rot.lon = vsr.res$rot.lon
    rot.lat = vsr.res$rot.lat
    visible = vsr.res$visible
    if (ignore.visibility) {
        visible[] = TRUE
    }
    xshift = plot.init.res$xshift
    yshift = plot.init.res$yshift
    if (any(visible)) {
        text(x = x[visible] + xshift, y = y[visible] + yshift, 
            labels = labels[visible], adj = adj, pos = pos, offset = offset, 
            vfont = vfont, cex = cex, col = col, font = font)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
