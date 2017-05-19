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
a spheRlab plot specifics list as returned by \code{sl.plot.init} (or a variant thereof).
}
  \item{lon}{
a scalar, vector, or array with \code{N} elements specifying the longitudes of the points.
}
  \item{lat}{
a scalar, vector, or array with \code{N} elements specifying the latitudes of the points.
}
  \item{col}{
a colour (or a vector or array thereof).
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
\code{sl.plot.lines}, \code{sl.plot.polygon}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
