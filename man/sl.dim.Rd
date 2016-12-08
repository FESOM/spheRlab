\name{sl.dim}
\alias{sl.dim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dimension of Array or Vector
}
\description{
Simple function to extend the functionality of \code{dim} to work also with vectors (returning a single integer in that case).
}
\usage{
sl.dim(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an array or vector with arbitrary (\code{N}) dimensions.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of length \code{N} with elements giving the sizes of the dimensions of \code{x}.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
sl.dim(array(1:24,dim=c(4,3,2)))
## Should return:
## [1] 4 3 2

sl.dim(1:24)
## Should return:
## [1] 24

## Note that the second example does not work if \code{dim} is used instead of \code{sl.dim}.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
