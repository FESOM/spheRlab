\name{sl.grid.writeCDO}
\alias{sl.grid.writeCDO}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write Grid for CDO
}
\description{
Write a spheRlab grid to a grid description file that makes unstructured-mesh data readable by the Climate Data Operators (CDO).
}
\usage{
sl.grid.writeCDO(grid, ofile = "~/sl.grid.CDO", netcdf = TRUE, netcdf.prec = "double", ascii.digits = Inf, overwrite = FALSE, verbose = TRUE, cell_area = TRUE, node_node_links = TRUE, triag_nodes = TRUE, coast = TRUE, depth = TRUE)
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
a logical value indicating whether or not to write directly to NetCDF (default). If \code{FALSE}, only the variables \code{lon}, \code{lon_bnds}, \code{lat}, and \code{lat_bnds} are written in an ascii format that can be coverted subsequently to NetCDF using CDO. The latter is much slower.
}
  \item{netcdf.prec}{
a character defining the precision at which the (non-integer) variables are written. Reasonable options are \code{"float"} and \code{"double"} (default). Used only if \code{netcdf=TRUE}.
}
  \item{ascii.digits}{
a scalar giving the number of digits after which coordinates are truncated (rounded). Used only if \code{netcdf=FALSE}.
}
  \item{overwrite}{
a logical value indicating whether or not to overwrite an already existing file.
}
  \item{verbose}{
a logical value indicating whether or not print statements shall report on the function progess.
}
  \item{cell_area}{
a logical value indicating whether or not to add cell (stamp polygon) areas (from \code{grid$cellareas}). Used only if \code{netcdf=TRUE}.
}
  \item{node_node_links}{
a logical value indicating whether or not to add node-node links (from \code{grid$neighnodes}). Used only if \code{netcdf=TRUE}.
}
  \item{triag_nodes}{
a logical value indicating whether or not to add triangular element triplets (from \code{grid$elem}). Used only if \code{netcdf=TRUE}.
}
  \item{coast}{
a logical value indicating whether or not to add coast information (from \code{grid$coast}). Used only if \code{netcdf=TRUE}.
}
  \item{depth}{
a logical value indicating whether or not to add a vector with the depth of each grid level (from \code{grid$depth}) and a vector that provides the number of active (wet) levels underneath each surface node (from \code{grid$depth.lev}). Used only if \code{netcdf=TRUE}.
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
%%
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.grid.readFESOM}}, \code{\link{sl.grid.addinfo}}
}
\examples{
\dontrun{
grid = sl.grid.readFESOM(griddir="PATH-TO-YOUR-FESOM-GRID",rot=TRUE,rot.invert=TRUE,rot.abg=c(50,15,-90))
sl.grid.writeCDO(grid,ofile="~/sl.grid.CDO")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line