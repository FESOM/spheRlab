\name{sl.grid.addinfo}
\alias{sl.grid.addinfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add Grid Variables to CDO Grid File
}
\description{
Add additional variables from a spheRlab grid list to a corresponding CDO grid file (NetCDF).
}
\usage{
sl.grid.addinfo(grid, ncdf.file.in, ncdf.file.out = paste0(ncdf.file.in,"_addinfo.nc"), add.cell_area = TRUE, add.node_node_links = TRUE, add.triag_nodes = TRUE, add.coast = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid}{
a spheRlab grid list as returned by \code{\link{sl.grid.readFESOM}}.
}
  \item{ncdf.file.in}{
a character specifying the path and file name of the file to which additional grid variables shall be added.
}
  \item{ncdf.file.out}{
a character specifying the path and file name of the output file.
}
  \item{add.cell_area}{
a logical value indicating whether or not to add cell (stamp polygon) areas.
}
  \item{add.node_node_links}{
a logical value indicating whether or not to add node-node links.
}
  \item{add.triag_nodes}{
a logical value indicating whether or not to add triangular element triplets.
}
  \item{add.coast}{
a logical value indicating whether or not to add coast information.
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
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.grid.readFESOM}}, \code{\link{sl.grid.writeCDO}}
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