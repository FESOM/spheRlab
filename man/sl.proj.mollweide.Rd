\name{sl.proj.mollweide}
\alias{sl.proj.mollweide}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mollweide Projection
}
\description{
Transform longitude/latitude coordinates according to approximated Mollweide projection (20th-order polynomial fit).
}
\usage{
sl.proj.mollweide(lon, lat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a scalar, vector, or array with \code{N} elements specifying the longitudes of the points.
}
  \item{lat}{
a scalar, vector, or array with \code{N} elements specifying the latitudes of the points.
}
}
\details{
Since the original Mollweide projection requires implicit/iterative solving and can thus be expensive for large data sets, this function approximates the Mollweide projection by a 20th-order polynomial fit.

This function is intended mainly for internal use by the spheRlab base plotting functions (\code{sl.plot.init()}, \code{sl.plot.lines()}, etc.).
}
\value{
a list with elements \code{x} and \code{y} providing the projected coordinates in the approximate ranges \code{-180<x<180} and \code{-90<y<90}.
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
\code{\link{sl.plot.init}}
}
\examples{
xy = sl.proj.mollweide(lon=seq(-180,180,30),lat=seq(-90,90,15))
plot(xy$x, xy$y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
