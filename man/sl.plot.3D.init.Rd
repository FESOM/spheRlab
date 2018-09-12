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
sl.plot.3D.init(projection = "polar", width = 30, lonlatrot.left = c(10, 0, 0), lonlatrot.right = c(-10, 0, 0), gap.frac = 0.1, col.background = NULL, precision = 1, device = "pdf", do.init = TRUE, do.init.device = do.init, file.name = paste0("~/sl.plot.3D.",device))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
  \item{col.background}{
a background colour. Default is \code{col.background=NULL}, meaning that no background is drawn. Note that this parameter determines only the backgound colour of the map(s). To set the background colour of the whole plot, use \code{par(bg=...)} before calling \code{sl.plot.3D.init}.
}
  \item{precision}{
a scalar specifying the precision (in degrees) at which the backgound polygon boundary is broken into straight line segments. Used only if \code{col.background != NULL}.
}
  \item{device}{
a character specifying which graphics device to use. Default is \code{device='pdf'}. Ignored if \code{do.init.device=FALSE}.
}
  \item{do.init}{
a logical value indicating whether or not an initial call of \code{plot} shall be executed. Default is \code{do.init=TRUE}.
}
  \item{do.init.device}{
a logical value indicating whether or not initialise a device as specified by \code{device}. Default is \code{do.init.device=do.init}. If \code{do.init.device=FALSE} but \code{do.init=TRUE}, the plot will be initialised in the standard graphical device.
}
  \item{file.name}{
a character specifying the output file. Only used if \code{do.init.device=TRUE}. Default is \code{file.name=paste0('~/sl.plot.3D.',device)}.
}
}
\details{
The list returned from this function must be provided to any spheRlab plotting functions called afterwards to fill the plot with data. After all plotting is done, \code{\link{sl.plot.end}} must be called, again with the list returned from this function provided.

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
\code{\link{sl.plot.end}}, \code{\link{sl.plot.init}}, \code{\link{sl.plot.platon.init}}
}
\examples{
## To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
