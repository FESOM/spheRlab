\name{sl.plot.lines}
\alias{sl.plot.lines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Lines
}
\description{
Plot lines into a spheRlab plot.
}
\usage{
sl.plot.lines(plot.init.res, lon, lat, col = "black", lwd = 1, lty = 1, ignore.visibility = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{sl.plot.init} (or a variant thereof).
}
  \item{lon}{
a vector of length \code{N} specifying the longitudes of the points defining the lines.
}
  \item{lat}{
a vector of length \code{N} specifying the latitudes of the points defining the lines.
}
  \item{col}{
a line colour.
}
  \item{lwd}{
a line width.
}
  \item{lty}{
a line type.
}
  \item{ignore.visibility}{
a logical value indicating whether or not the line shall be attempted to be drawn completely even if it's classified partly or fully invisible, that is, outside the plot domain.
}
}
\details{
If \code{lon} and \code{lat} contain no NAs, \code{N-1} lines defined by any adjacent pairs of coordinates are drawn. Separate line segments can be drawn with a single call of \code{sl.plot.lines} by concatenating their coordinate vectors with NAs inbetween.

This is one of the lowest-level plotting functions of spheRlab. First the visibility of the points is checked, which depends on the projection and plot domain boundaries. Lines for which both end points are invisible are not drawn. (This means that lines where only a middle segment is visible are not drawn!) Then for partly visible lines an intersection point with the boundary is computed and only the visible part is drawn. Of course the completely visible lines are drawn as well.
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
\code{sl.plot.polygon}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
