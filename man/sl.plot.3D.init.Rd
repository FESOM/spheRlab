\name{sl.plot.3D.init}
\alias{sl.plot.3D.init}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
3D Polar Plot
}
\description{
Initialise a spheRlab 3D polar plot. Two panels are drawn next to each other, rotated slightly such that a 3D effect occurs when the panels are overlaid, by aiming with the eyes at a point either before or behind the plane where the figure is displayed.
}
\usage{
sl.plot.3D.init(file.name = "~/sl.plot.pdf", projection = "polar", width = 30, lonlatrot.left = c(10, 0, 0), lonlatrot.right = c(-10, 0, 0), gap.frac = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.name}{
  a character specifying the output file.
}
  \item{projection}{
  a character specifying the plot projection to be used. At the moment only 'polar' is implemented (the default).
}
  \item{width}{
  a scalar specifying the width of the resulting figure.
}
  \item{lonlatrot.left}{
  a vector of length 3 specifying the longitude and latitude of the projection north pole and the amount of rotation in degrees for the left panel.
}
  \item{lonlatrot.right}{
  a vector of length 3 specifying the longitude and latitude of the projection north pole and the amount of rotation in degrees for the right panel.
}
  \item{gap.frac}{
  a scalar specifying the relative width of the gap between the two panels.
}
}
\details{
The list returned from this function must be provided to any spheRlab plotting functions called afterwards to fill the plot with data. After all plotting is done, \code{sl.plot.end} must be called, again with the list returned from this function provided.

A displacement of the two poles by about 20 degrees (as by default) may be appropriate.
}
\value{
A list that must be provided to any spheRlab plotting functions called afterwards. In fact, the returned list contains two standard spheRlab plot specifics lists as the first two (unnamed) elements for the two panels that will be generated for the 3D plot, plus a third list element \code{...$projection="3D"}.
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
\code{sl.plot.end}, \code{sl.plot.init}, \code{sl.plot.platon.init}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
