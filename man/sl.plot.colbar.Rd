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
sl.plot.colbar(colbar, categorical = FALSE, breaks = NULL, vertical = TRUE, labels.at = NULL, labels.num = min(length(breaks),5), labels = NULL, labels.side = "bottom", labels.cex = 1, labels.signif = 3, ticks.length = 1, ticks.mirrored = FALSE, ratio = 0.1, triag.ends = FALSE, device = "pdf", do.init = TRUE, do.init.device = TRUE, do.close.device = do.init.device, file.name = paste0("~/sl.plot.colbar.",device), width = 6, xshift = 0, yshift = 0, len = 0.6, units = NULL, units.xy = NULL, labels.col = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colbar}{
  a spheRlab colourbar of length \code{N} (as returned by \code{\link{sl.colbar}}), or a list including such a colourbar as an element named 'colbar'. In the latter case, the arguments \code{categorical}, \code{breaks}, and \code{labels} are ignored and taken from \code{colbar} if \code{colbar} also includes accordingly named elements.
}
  \item{categorical}{
  a logical value specifying whether the colourbar is categorical. If \code{TRUE}, labels are placed at colour centres. Default is \code{FALSE}. Ignored if \code{colbar} is a list with elements 'colbar' as well as 'categorical'.
}
  \item{breaks}{
  a vector of length \code{N-1} specifying the breaks corresponding to \code{colbar}. Ignored if \code{categorical=TRUE}. Ignored if \code{colbar} is a list with elements 'colbar' as well as 'breaks'.
}
  \item{vertical}{
  a logical value specifying whether the colourbar is to be plotted vertically (default). If \code{vertical=FALSE}, a horizontal colourbar is plotted.
}
  \item{labels.at}{
  \code{NULL} or a vector of length \code{N-1} (or \code{N} if \code{categorical=TRUE}) with logical values specifying which breaks (or colours) are to be labelled. If \code{NULL} (default) and \code{categorical=FALSE}, the argument \code{labels.num} is used to place labels automatically in a quasi equidistant manner. If \code{NULL} (default) and \code{categorical=TRUE}, each colour is labelled.
}
  \item{labels.num}{
  a scalar specifiying how many labels are to be drawn in a quasi equidistant manner. Used only if \code{labels.at=NULL} and \code{categorical=FALSE}. Default is \code{labels.num=min(length(breaks),9)}. To plot labels at all breaks, set \code{labels.num=NULL} (as well as \code{labels.at=NULL}).
}
  \item{labels}{
  \code{NULL} or a vector specifying the labels. If \code{categorical=FALSE}, the length must be \code{N-1} and the labels correspond to the colour breaks; if \code{NULL} (default), labels are derived from \code{breaks}. If \code{categorical=TRUE}, the length must be \code{N} and the labels correspond to colour centres; if \code{NULL} (default), labels are \code{1:N}. Note that this vector has the same length as \code{breaks} (or \code{colbar}), meaning that it must include pseudo entries even for breaks (or colours) that are not labelled. Ignored if \code{colbar} is a list with elements 'colbar' as well as 'labels'.
}
  \item{labels.side}{
  NOT YET USED! - a character specifying the side of the colourbar at which the labels are to be placed.
}
  \item{labels.cex}{
  a scalar specifying the relative size of the labels.
}
  \item{labels.signif}{
  an integer specifying the number of significant digits to be printed for the labels. Used only if \code{categorical=FALSE}. Default is \code{labels.signif=3}.
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
  a logical value indicating whether the ends of the colourbar (that is, the last colour polygons) shall be triangular. By default \code{triag.ends=FALSE}.
}
  \item{device}{
a character specifying which graphics device to use. Default is \code{device='pdf'}. Ignored if \code{do.init.device=FALSE}.
}
  \item{do.init}{
a logical value indicating whether or not an initial call of \code{plot} shall be executed. Default is \code{do.init=TRUE}.
}
  \item{do.init.device}{
a logical value indicating whether or not to initialise a device as specified by \code{device}. Default is \code{do.init.device=do.init}. If \code{do.init.device=FALSE} but \code{do.init=TRUE}, the plot will be initialised in the standard graphical device.
}
  \item{do.close.device}{
a logical value specifying whether to call \code{dev.off()} to close the corresponding device in the end. Default is \code{do.close.device=do.init.device}.
}
  \item{file.name}{
  a character specifying the output file name of the figure. By default \code{file.name=paste0("~/sl.plot.colbar.",device)}.
}
  \item{width}{
  a scalar specifying the width of the figure.
}
  \item{xshift}{
  a scalar specifying how far the colourbar shall be shifted from the plot coordinate origin in x-direction. Default is \code{xshift=0}.
}
  \item{yshift}{
  a scalar specifying how far the colourbar shall be shifted from the plot coordinate origin in y-direction. Default is \code{yshift=0}.
}
  \item{len}{
  a scalar specifying the (absolute) length of the colourbar. Default is \code{0.6}. If \code{do.init=TRUE}, in which case the plot domain ranges from -0.5 to 0.5 in both x and y direction, \code{len} also corresponds to the colourbar length relative to the plot size.
}
  \item{units}{
  a character specifying a units string to be added to the plot. Default is \code{""}, in which case no units are added.
}
  \item{units.xy}{
  a two-element numeric vector specifying the relative (scaled by \code{len}) x and y coordinates of the units relative to the colourbar. If not specified, \code{units.xy=c(0,1.2)} for vertical colourbars (units are added above the upper end) and \code{units.xy=c(1.2,0)} for vertical colourbars (units are added next to the right end).
}
  \item{labels.col}{
  the colour of the labels (and units).
}
}
\details{
By default, colourbars in spheRlab follow the philosophy that they are plotted separately of the fields they annotate. In this case, combining a plotted field and its colourbar subsequently is left to the user and his or her choice of external software (e.g., OpenOffice or the like). This increases the simplicity and flexibility of plot handling within spheRlab.

It is also possible to add a colourbar to an existing plot by setting \code{do.init=FALSE} and adjusting \code{xshift}, \code{yshift}, and \code{len}. If adding to a spheRlab plot initialised with \code{\link{sl.plot.init}} or variants thereof, note that the plot x-y-ranges are stored in the list returned by the initialising function call in the elements \code{xlim} and \code{ylim}.
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
This function still has some unused arguments that should be implemented at some point.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.colbar}}, \code{\link{sl.num2colbar}}, \code{\link{sl.view.colbar}}
}
\examples{
cb = sl.colbar(cols=rgb(c(1,0,0),c(.5,.5,.5),c(0,0,1)),N=10)
breaks = seq(.1,.9,.1)
sl.plot.colbar(colbar=cb,breaks=breaks,do.init.device=F,units="(K)")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
