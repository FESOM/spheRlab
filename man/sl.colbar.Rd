\name{sl.colbar}
\alias{sl.colbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a Custom Colourbar
}
\description{
Generate a colourbar of length \code{N} from a series of >= 2 colours with linear interpolation in RGB(alpha)-space between the colours. The provided colours can be placed not only in equidistant steps (default), but also in non-equidistant steps.
}
\usage{
sl.colbar(cols, N = (length(cols) - 1) * 10 + 1, cols.at = seq(0, 1, by = 1/(length(cols) - 1)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cols}{
either a list of (at least 2) RGB(alpha) tuples (length-3 or length-4 vectors in the range [0,1]) or a character vector of named colours and/or hexadecimal-coded RGB colours. Defines the basis colours of the colourbar.
}
  \item{N}{
number of colours the resulting colourbar shall contain.
}
  \item{cols.at}{
a vector of length \code{N} that specifies at which relative locations the basis colours are placed. The first and last elements should be \code{0} and \code{1}, and the remaining should have values inbetween in ascending order. By default, the basis colours are placed in equidistant steps.
}
}
\details{
Can also use RColorBrewer colourbars as input, see corresponding examples below.
}
\value{
  A list of length \code{N} with characters describing RGB(alpha) values (hexadecimal) of the colours comprising the colourbar.
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
\code{\link{sl.view.colbar}}, \code{\link{sl.plot.colbar}}, \code{\link{sl.num2colbar}}, \code{\link{sl.num2colbarbreaks}}
}
\examples{
sl.colbar(cols=list(c(1,0,0),c(1,1,1),c(0,0,1)),N=5)
## Should return:
## [[1]]
## [1] "#FF0000"
## 
## [[2]]
## [1] "#FF8080"
## 
## [[3]]
## [1] "#FFFFFF"
## 
## [[4]]
## [1] "#8080FF"
## 
## [[5]]
## [1] "#0000FF"

require("RColorBrewer")
## For a list of all available Color Brew colourbars, type display.brewer.all()
sl.colbar(cols=brewer.pal(5,"BrBG"),N=5)
## Should return:
## [[1]]
## [1] "#A6611A"
## 
## [[2]]
## [1] "#DFC27D"
## 
## [[3]]
## [1] "#F5F5F5"
## 
## [[4]]
## [1] "#80CDC1"
## 
## [[5]]
## [1] "#018571"

## Using a different 'N' (as by default), colours are interpolated inbetween, e.g.:
sl.view.colbar(sl.colbar(cols=brewer.pal(11,"BrBG")))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
