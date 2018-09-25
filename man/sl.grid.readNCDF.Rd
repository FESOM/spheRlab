\name{sl.grid.readNCDF}
\alias{sl.grid.readNCDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read NetCDF FESOM Grid
}
\description{
Read a NetCDF FESOM grid using the \code{ncdf4} library.
}
\usage{
sl.grid.readNCDF(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
  a character specifying the path and name of the NetCDF FESOM grid description file.
}
}
\value{
A list with all variables contained in the grid description file.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.grid.readFESOM}}, \code{\link{sl.grid.writeCDO}}
}
\examples{
# To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
