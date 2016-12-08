\name{sl.crossvec}
\alias{sl.crossvec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cross Product
}
\description{
Compute the cross product of two 3-dimensional vectors.
}
\usage{
sl.crossvec(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
a vector of length 3 (x,y,z coordinates).
}
  \item{b}{
a vector of length 3 (x,y,z coordinates).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of length 3 (x,y,z coordinates) providing the cross product of \code{a} and \code{b}.
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
sl.crossvec(a=c(1,0,0),b=c(0,1,0))
## Should return:
## [1] 0 0 1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
