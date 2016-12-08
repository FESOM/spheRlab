\name{sl.plot.domainbound.curvilin}
\alias{sl.plot.domainbound.curvilin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Curvilinear Grid Boundaries
}
\description{
Plot the outer boundaries of a curvilinear regional grid, based on matrices specifying the mesh interface longitudes and latitudes.
}
\usage{
sl.plot.domainbound.curvilin(plot.init.res, lon.i, lat.i, col = "black", lwd = 1, lty = 1, ignore.visibility = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot.init.res}{
a spheRlab plot specifics list as returned by \code{sl.plot.init} (or a variant thereof).
}
  \item{lon.i}{
a matrix specifying the interface longitudes of the grid.
}
  \item{lat.i}{
a matrix specifying the interface latitudes of the grid.
}
  \item{col}{
the line colour.
}
  \item{lwd}{
the line width.
}
  \item{lty}{
the line type.
}
  \item{ignore.visibility}{
a logical value specifying if visibility shall be ignored (for example, if the coordinates of a plot object are on the far side of the sphere in polar projection).
}
}
\details{
%%
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
Implemented originally to visualise of CORDEX domains (e.g. on platonian body plots).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
