\name{sl.plot.colbar}
\alias{sl.plot.colbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Colourbar
}
\description{
Plot a spheRlab colourbar, vertically or horizontally, with a number of options.
}
\usage{
sl.plot.colbar(colbar, breaks, vertical = FALSE, labels.at = rep(TRUE, length(breaks)), labels = as.character(signif(breaks, 3)), labels.side = "bottom", labels.cex = 1, ticks.length = 1, ticks.mirrored = FALSE, ratio = 0.1, triag.ends = FALSE, file.name = "~/sl.plot.colbar.pdf", width = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colbar}{
  A spheRlab colourbar of length \code{N} (as returned by \code{sl.colbar}).
}
  \item{breaks}{
  a vector of length \code{N-1} specifying the breaks corresponding to \code{colbar}.
}
  \item{vertical}{
  a logical value specifying whether the colourbar is to be plotted vertically (default). If \code{vertical=FALSE}, a horizontal colourbar is plotted.
}
  \item{labels.at}{
  a vector of length \code{N-1} with logical values specifying which breaks are to be labelled. By default all breaks are labelled (which can result in unwanted overlap).
}
  \item{labels}{
  a vector of length \code{N-1} with characters specifying the break labels. By default, labels are derived from \code{breaks}.
}
  \item{labels.side}{
  NOT YET USED! - a character specifying the side of the colourbar at which the labels are to be placed.
}
  \item{labels.cex}{
  a scalar specifying the relative size of the labels.
}
  \item{ticks.length}{
  NOT YET USED!
}
  \item{ticks.mirrored}{
  NOT YET USED!
}
  \item{ratio}{
  a scalar specifying the ratio of width and length of the colourbar. By default \code{ratio=0.1}.
}
  \item{triag.ends}{
  a logical value indicating whether the ends of the colourbar shall be triangular. By default \code{triag.ends=FALSE}. TRIANGULAR ENDS ARE NOT YET IMPLEMENTED!
}
  \item{file.name}{
  a character specifying the output file name of the figure. By default \code{file.name="~/sl.plot.colbar.pdf"}.
}
  \item{width}{
  a scalar specifying the width of the figure.
}
}
\details{
Colourbars in spheRlab follow the philosophy that they are plotted separately of the fields they annotate. Combining a plotted field and its colourbar subsequently is left to the user and his or her choice of external software (e.g., OpenOffice or the like). This substantially increases the simplicity and flexibility of plot handling within spheRlab.
}
\value{
An integer giving the device into which the figure was plotted.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
NOTE THAT SO FAR THIS FUNCTION HAS UNUSED ARGUMENTS AND OTHER GAPS (INCLUDING HORIZONTAL PLOTTING) THAT NEED TO BE FILLED ASAP!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
cb = sl.colbar(cols=list(c(1,0,0),c(.5,.5,.5),c(0,0,1)),N=10)
breaks = seq(.1,.9,.1)
## sl.plot.colbar(colbar=cb,breaks=breaks,file.name="~/sl.plot.colbar.pdf")
## Should result in a corresponding figure stored in the file '~/sl.plot.colbar.pdf'
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
