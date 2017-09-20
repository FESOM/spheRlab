\name{sl.num2colbarbreaks}
\alias{sl.num2colbarbreaks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Auto-Define Colourbar Breaks
}
\description{
Automatically define a discrete number of breaks given a numerical vector or matrix for application with a discrete colourbar.
}
\usage{
sl.num2colbarbreaks(num, colbar = NA, Nbreaks = NA, breaks.log = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{
  a numerical vector or matrix.
}
  \item{colbar}{
  a spheRlab colourbar of length \code{N}, that is, a list of characters giving colours as produced by \code{\link{sl.colbar}} (which uses the function \code{rgb} to generate the individual colours). This argument is optional; instead, just the number of breaks can be specified via \code{Nbreaks}.
}
  \item{Nbreaks}{
  an integer giving the number of breaks to be generated. Note that \code{Nbreaks} corresponds to \code{N-1}. Used only if \code{colbar} is not specified.
}
  \item{breaks.log}{
  a logical value indicating whether the breaks shall be placed in logarithmic steps. Default is \code{breaks.log=FALSE}.
}
}
\details{
This function is used by \code{\link{sl.num2colbar}}.
}
\value{
A vector of length \code{N-1} giving the breaks.
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
\code{\link{sl.num2colbar}}
}
\examples{
cb = sl.colbar(cols=list(c(1,0,0),c(.5,.5,.5),c(0,0,1)),N=5)
sl.num2colbarbreaks(num=runif(n=1000,min=-5,max=5),colbar=cb)
## Should return something like (result subject to randomness):
## [1] -3.0019879 -1.0079374  0.9861131  2.9801636
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
