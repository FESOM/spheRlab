\name{sl.xyz.angle}
\alias{sl.xyz.angle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute Angle Between 3D Vectors
}
\description{
Compute the angle between two 3D vectors.
}
\usage{
sl.xyz.angle(a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{
a vector of length 3 specifying the x-y-z components of the first vector.
}
  \item{b}{
a vector of length 3 specifying the x-y-z components of the second vector.
}
}
\details{
This function is based on the scalar product using the operator \code{\%*\%}.
}
\value{
A scalar giving the angle between the two vectors in degrees.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
%
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{sl.lonlat.angle}
}
\examples{
sl.xyz.angle(c(1,0,0),c(0,0,1))
## Should return:
## [1] 90

sl.xyz.angle(c(1,0,0),c(0,0,2))
## Should return:
## [1] 90
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
