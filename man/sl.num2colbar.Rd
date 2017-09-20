\name{sl.num2colbar}
\alias{sl.num2colbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Numerics to Integers (Colourbar Breaks)
}
\description{
Convert a numerical vector or array to integers corresponding to discrete intervals defined by \code{breaks}. The latter can be provided as argument, or they are computed by calling \code{\link{sl.num2colbarbreaks}}). This function is used for plotting numerical fields where polygons are coloured according to a colourbar with colours assigned to discrete intervals.
}
\usage{
sl.num2colbar(num, colbar = sl.colbar.blackwhite_256, breaks = NA, breaks.log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{
%%     ~~Describe \code{num} here~~
}
  \item{colbar}{
%%     ~~Describe \code{colbar} here~~
}
  \item{breaks}{
%%     ~~Describe \code{breaks} here~~
}
  \item{breaks.log}{
%%     ~~Describe \code{breaks.log} here~~
}
}
\details{
This function is used by \code{\link{sl.num2colbarbreaks}}, \code{\link{sl.plot.elem}}, \code{\link{sl.plot.field}}, and \code{\link{sl.plot.fld.curvilin}}.
}
\value{
\item{colour.index}{
  A vector or array with the same dimensions as the argument \code{num}, converted to integers corresponding to discrete intervals defined by \code{breaks} (which were either provided as argument or computed by calling \code{\link{sl.num2colbarbreaks}}).
}
\item{breaks}{
  A vector giving the \code{breaks} (particularly relevant if these were not provided as argument but computed by calling \code{\link{sl.num2colbarbreaks}}).
}
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
\code{\link{sl.colbar}}, \code{\link{sl.num2colbarbreaks}}.
}
\examples{
cb = sl.colbar(cols=list(c(1,0,0),c(.5,.5,.5),c(0,0,1)),N=5)
num = runif(n=100,min=-5,max=5)
cb_breaks = sl.num2colbarbreaks(num=num,colbar=cb)
sl.num2colbar(num=num,colbar=cb)
## Should return something like (result subject to randomness)
## $colour.index
## [1] 1 5 4 4 1 2 2 2 2 3 1 2 4 5 2 1 1 4 2 1 1 2 4 4 3 4 2 3 5 1 3 4 5 2 5 5 1 2 2
## [40] 5 1 2 3 4 3 1 4 4 2 1 5 1 5 4 2 1 2 2 3 3 2 4 4 4 2 2 3 1 1 4 4 3 2 1 5 2 3 1
## [79] 2 3 1 5 2 3 3 4 4 4 5 3 2 3 4 2 3 5 2 4 3 1
## 
## $breaks
## [1] -2.9800799 -0.9964841  0.9871118  2.9707076
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
