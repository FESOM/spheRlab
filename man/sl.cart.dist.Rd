\name{sl.cart.dist}
\alias{sl.cart.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cartesian Distance
}
\description{
Compute Cartesian distance between two points in any dimension.
}
\usage{
sl.cart.dist(a, b = NULL, norm = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
a scalar or numerical vector of any length giving the location of one point
}
  \item{b}{
a scalar or numerical vector of the same length as \code{a} giving the location of the second point. If not specified, the distance of \code{a} from the origin is computed.
}
  \item{norm}{
With the default value \code{norm=2} the Euclidean distance is computed, but any other norm can be specified.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Single scalar providing the distance.
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
\code{\link{sl.gc.dist}}
}
\examples{
## Example in 3 dimensional space:
sl.cart.dist(a=c(0,0,0),b=c(1,1,1))
## Should return:
## [1] 1.732051
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
