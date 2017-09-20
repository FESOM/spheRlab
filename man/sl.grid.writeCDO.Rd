\name{sl.grid.writeCDO}
\alias{sl.grid.writeCDO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write Grid for CDO
}
\description{
Write a spheRlab grid to a file that is readable by the Climate Data Operators (CDO). Currently only ascii output is implemented.
}
\usage{
sl.grid.writeCDO(grid, ofile = "~/sl.grid.CDO", netcdf = FALSE, write.precision = Inf, overwrite = FALSE, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
a spheRlab grid list as returned by \code{\link{sl.grid.readFESOM}}.
}
  \item{ofile}{
a character specifying the path and file name of the output file.
}
  \item{netcdf}{
NOT YET IMPLEMENTED - a logical value indicating whether or not to write directly to NetCDF instead of writing to ascii with subsequent conversion using CDO.
}
  \item{write.precision}{
a scalar giving the number of digits after which coordinates are truncated (rounded).
}
  \item{overwrite}{
a logical value indicating whether or not to overwrite an already existing file.
}
  \item{verbose}{
a logical value indicating whether or not print statements shall report on the function progess.
}
}
\details{
This is a special function that is typically used when converting FESOM grid information to a CDO-readable format, see the example below.
}
\value{
%
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
Currently only ascii output is implemented. However, it would be much faster to write directly to NetCDF instead of writing to ascii with subsequent conversion using CDO. This is particularly the case for very large FESOM grids.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.grid.readFESOM}}, \code{\link{sl.grid.addinfo}}
}
\examples{
\dontrun{
grid = sl.grid.readFESOM(griddir="PATH-TO-YOUR-FESOM-GRID",rot=TRUE,rot.invert=TRUE,rot.abg=c(50,15,-90))
ofile = "~/sl.grid.CDO"
sl.grid.writeCDO(grid,ofile=ofile)
system(paste0("cdo -f nc const,0,",ofile," ",ofile,".nc"))
sl.grid.addinfo(grid,ncdf.file.in=paste0(ofile,".nc"))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line