\name{sl.view.colbar}
\alias{sl.view.colbar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
View Colourbar
}
\description{
View a spheRlab colour bar (by simple plotting).
}
\usage{
sl.view.colbar(colbar, labels = TRUE, labels.cex = 1, labels.col = "black")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{colbar}{
a spheRlab colourbar as returned by \code{sl.colbar}, that is, a list of length \code{N} with characters describing RGB values (hexadecimal) of the colours comprising the colourbar.
}
  \item{labels}{
a logical value indicating whether or not the colour indices are added as labels.
}
  \item{labels.cex}{
a scalar that can be used to increase of decrease the label size.
}
  \item{labels.col}{
the label colour.
}
}
\details{
This function allows to view a colourbar, independent of how it's applied to a numerical field with corresponding numerical breaks. A simple 'matrix' of the colours is plotted using \code{plot} and \code{rect}. In contrast to other spheRlab plotting functions, this one is not meant to produce a high-quality figure (in pdf format), but for a quick view.
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
\code{sl.colbar}, \code{sl.plot.colbar}
}
\examples{
\dontrun{
cb = sl.colbar(cols=list(c(1,0,0),c(1,1,1),c(0,0,1)),N=11)
sl.view.colbar(cb)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
