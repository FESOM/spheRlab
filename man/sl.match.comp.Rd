\name{sl.match.comp}
\alias{sl.match.comp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find First Comparison Match
}
\description{
A simple function returning the index \code{i} of the first element of a vector \code{table} for which \code{table[i] COMP x == TRUE}, where \code{x} is a scalar and \code{COMP} is one of a number of comparison operators that can be specified.
}
\usage{
sl.match.comp(x, table, comp.op = "gt", offset = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  a scalar to be compared with the elements of \code{table}.
}
  \item{table}{
  a vector whose elements are to be compared with \code{x}.
}
  \item{comp.op}{
  a character giving a comparison operator. Must be one of 'gt' ('greater than'; default), 'lt' (less than), 'get' (greater than or equal with), 'let' (less than or equal with), and 'eq' (equal with).
}
  \item{offset}{
  an integer indicating how many indices to skip at the beginning.
}
}
\details{
Note that the function works with matrices, too, but in an unexpected manner, returning a one-dimensional index.
}
\value{
An integer giving the index of the first element (optionally after skipping a number of elements) for which the comparison is \code{TRUE}.
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
sl.match.comp(x=7.2,table=seq(1,10),comp.op="gt")
## Should return:
## [1] 8
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
