\name{sl.num2colbar}
\alias{sl.num2colbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Numerics to Integers Corresponding to Colourbar Breaks
}
\description{
Convert a numerical vector or array to integers corresponding to discrete intervals defined by colourbar breaks. If the latter are not provided (as argument \code{breaks} or as an element of \code{colbar} named 'breaks'), they are computed by calling \code{\link{sl.num2colbarbreaks}}). This function is used by various plotting functions.
}
\usage{
sl.num2colbar(num, colbar = NULL, breaks = NA, breaks.log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{
a numerical scalar, vector, or array that shall be mapped to colourbar breaks
}
  \item{colbar}{
a spheRlab colourbar of length \code{N} (as returned by \code{\link{sl.colbar}}), or a list including such a colourbar as an element named 'colbar'. In the latter case, if \code{colbar} also includes an element named 'breaks', the argument \code{breaks} is ignored and taken from \code{colbar}. If \code{breaks} are provided, this argument is optional.
}
  \item{breaks}{
a vector of length \code{N-1} specifying the breaks corresponding to \code{colbar}. Ignored if \code{colbar} is a list with elements 'colbar' as well as 'breaks'.
}
  \item{breaks.log}{
a logical value indicating whether the breaks shall be placed in logarithmic steps. Default is \code{breaks.log=FALSE}. Only used if \code{breaks} is not provided, neither directly nor as part of \code{colbar}.
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
