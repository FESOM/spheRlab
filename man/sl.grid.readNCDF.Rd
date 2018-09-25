\name{sl.grid.readNCDF}
\alias{sl.grid.readNCDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read NetCDF Grid
}
\description{
Read a NetCDF grid (e.g. a FESOM grid) using the \code{ncdf4} library.
}
\usage{
sl.grid.readNCDF(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
  a character specifying the path and name of the NetCDF grid description file.
}
}
\value{
A list with all variables contained in the grid description file.
}
\note{
When reading very large grids and not all grid variables are needed (e.g., only centre lon-lats and not the larger-size corresponding bounds), it might be advisable (because faster) to read only required fields. To do so, use the \code{ncdf4} library directly with \code{\link{nc_open}} and \code{\link{ncvar_get}} (also see the code of \code{\link{sl.grid.readNCDF}}).
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.grid.readFESOM}}, \code{\link{sl.grid.writeCDO}}, \code{\link{sl.grid.writeFESOM}}
}
\examples{
# To be provided ...
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
