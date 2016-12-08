\name{sl.plot.contours}
\alias{sl.plot.contours}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Contours
}
\description{
Add contour lines to a spheRlab plot.
}
\usage{
sl.plot.contours(plot.init.res, contours.res, indices = NULL, col = "black", lwd = 1, lty = 1, label.density = 1, cex.labels = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{sl.plot.init} (or a variant thereof).
}
  \item{contours.res}{
a list of contours consisting of segments, as returned by \code{sl.contours}.
}
  \item{indices}{
an integer vector with indices indicating which contours (levels) of the contours list shall be plotted. If not specified, all contours are plotted.
}
  \item{col}{
A vector specifying the colours of the contour lines. If there are more contours than colours specified, the colours are recycled.
}
  \item{lwd}{
A vector specifying the line widths of the contour lines. If there are more contours than lines widths specified, the line widths are recycled.
}
  \item{lty}{
A vector specifying the line types of the contour lines. If there are more contours than lines types specified, the line types are recycled.
}
  \item{label.density}{
NOT YET USED. LABELLING TO BE IMPLEMENTED.
}
  \item{cex.labels}{
NOT YET USED. LABELLING TO BE IMPLEMENTED.
}
}
\details{
The function uses \code{sl.plot.lines} to plot each segment of each contour line contained in \code{contours.res}.
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
Automatic labelling shall be implemented at some point, making use at least of the two unused arguments.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{sl.plot.lines}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
