\name{sl.findnn.curvilin}
\alias{sl.findnn.curvilin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Nearest Neighbour in a Gridded Field
}
\description{
Find the nearest neighbour of a gridded field relative to a specific point on a sphere.
}
\usage{
sl.findnn.curvilin(lon = NULL, lat = NULL, x = NULL, y = NULL, z = NULL, lon.grid = NULL, lat.grid = NULL, x.grid = NULL, y.grid = NULL, z.grid = NULL, bruteforce = FALSE, Rsphere = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
a scalar or numeric vector of length \code{L} with the longitude(s) of the point(s) for which to find the nearest neighbour(s). Not required if \code{(x.grid,y.grid,z.grid)} are provided.
}
  \item{lat}{
a scalar or numeric vector of length \code{L} with the latitude(s) of the point(s) for which to find the nearest neighbour(s). Not required if \code{(x.grid,y.grid,z.grid)} are provided.
}
  \item{x}{
a scalar or numeric vector of length \code{L} with the x-coordinates of the point(s) for which to find the nearest neighbour(s). Note that \code{(x,y,z)} must be on a unit sphere, that is, \code{sqrt(x^2+y^2+z^2)=1} must hold. \code{(x,y,z)} are not required if \code{(lon,lat)} are provided.
  }
  \item{y}{
a scalar or numeric vector of length \code{L} with the y-coordinates of the point(s) for which to find the nearest neighbour(s). Note that \code{(x,y,z)} must be on a unit sphere, that is, \code{sqrt(x^2+y^2+z^2)=1} must hold. \code{(x,y,z)} are not required if \code{(lon,lat)} are provided.
  }
  \item{z}{
a scalar or numeric vector of length \code{L} with the z-coordinates of the point(s) for which to find the nearest neighbour(s). Note that \code{(x,y,z)} must be on a unit sphere, that is, \code{sqrt(x^2+y^2+z^2)=1} must hold. \code{(x,y,z)} are not required if \code{(lon,lat)} are provided.
  }
  \item{lon.grid}{
an \code{MxN} matrix with the longitudes of the points of a curvilinear grid. Not required if \code{(x.grid,y.grid,z.grid)} are provided.
}
  \item{lat.grid}{
an \code{MxN} matrix with the latitudes of the points of a curvilinear grid. Not required if \code{(x.grid,y.grid,z.grid)} are provided.
}
  \item{x.grid}{
an \code{MxN} matrix with the x-coordinates of the points of a curvilinear grid. Note that \code{(x.grid,y.grid,z.grid)} must be on a unit sphere, that is, \code{sqrt(x.grid^2+y.grid^2+z.grid^2)=1} must hold. \code{(x.grid,y.grid,z.grid)} are not required if \code{(lon.grid,lat.grid)} are provided.
}
  \item{y.grid}{
an \code{MxN} matrix with the y-coordinates of the points of a curvilinear grid. Note that \code{(x.grid,y.grid,z.grid)} must be on a unit sphere, that is, \code{sqrt(x.grid^2+y.grid^2+z.grid^2)=1} must hold. \code{(x.grid,y.grid,z.grid)} are not required if \code{(lon.grid,lat.grid)} are provided.
}
  \item{z.grid}{
an \code{MxN} matrix with the z-coordinates of the points of a curvilinear grid. Note that \code{(x.grid,y.grid,z.grid)} must be on a unit sphere, that is, \code{sqrt(x.grid^2+y.grid^2+z.grid^2)=1} must hold. \code{(x.grid,y.grid,z.grid)} are not required if \code{(lon.grid,lat.grid)} are provided.
}
  \item{bruteforce}{
a logical value specifying whether to find the nearest neighbour(s) by computing the distance(s) from all grid points. If \code{FALSE} (default) and if \code{max(M,N)>5}, the nearest neighbour(s) is (are) found in an iterative manner (see Details), which can be much faster for large grids.
  }
  \item{Rsphere}{
a scalar specifying the radius of the sphere to be used when computing the distance; default is \code{Rsphere=1}. This affects only the return value(s); if the point(s) or the grid are provided in Cartesian coordinates, these must correspond to a unit sphere in any case.
  }
}
\details{
If \code{FALSE} (default) and if \code{max(M,N)>5}, the nearest neighbour(s) is (are) found in an iterative manner by subsetting (coarsening) the grid such that distances are first computed only for \code{5x5} quasi-evenly distributed points (using the indices \code{[round(seq(0,4)*((M-1)/4))+1),round(seq(0,4)*((N-1)/4))+1)]}). In the next step, another \code{5x5} subset refined by a factor \code{2}, centered on the nearest neighbour of the previous subset, is used. This procedure is continued until the subgrid resolution reaches the original grid resolution (in both dimensions), which will have 'zoomed' into the location of the overall nearest neighbour. This iterative procedure can be much faster for large grids because distances don't need to be computed from all grid points (scaling much better).
}
\value{
  a list with three elements:
  \item{nn.index}{
  an \code{Lx2} data frame with columns named 'row' and 'col' for the indices of the nearest neighbour(s). If \code{L=1}, it might be convenient in some cases to convert the data frame into a vector, e.g., using \code{unlist()}.
  }
  \item{nn.dist}{
  a scalar or numeric vector of length \code{L} providing the distance from the respective nearest neighbour.
  }
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
\code{\link{sl.grid2path}}
}
\examples{
# generate an example curvilinear grid; first just lon-lat, then rotate (to around the North Pole) to make it more general
lon.seq = seq(-60,60,5)
lat.seq = seq(-30,30,5)
Nx = length(lon.seq)
Ny = length(lat.seq)
lon.0 = matrix(rep(lon.seq,Ny),nrow=Nx)
lat.0 = matrix(rep(lat.seq,each=Nx),nrow=Nx)
abg = sl.lonlatrot2abg(c(0,0,30))
rot.lonlat = sl.rot(lon = lon.0, lat = lat.0, alpha = abg[1], beta = abg[2], gamma = abg[3])
lon.grid = matrix(rot.lonlat$lon,nrow=Nx)
lat.grid = matrix(rot.lonlat$lat,nrow=Nx)

# define a few points, 'spiralling' to the North Pole
points.lon = seq(-135,180,45)
points.lat = seq(50,85,5)

# compute the nearest neighbours
nn = sl.findnn.curvilin(lon = points.lon, lat = points.lat, lon.grid = lon.grid, lat.grid = lat.grid)

# visualize the grid, points, and nearest neighbours in a map
pir = sl.plot.init(projection = "polar", do.init.device = FALSE)
sl.plot.naturalearth(pir, what = "land", resolution = "coarse", fill.col = "grey")
sl.plot.lonlatgrid(pir, col = "black", pole.hole = TRUE)
sl.plot.points(pir, lon = lon.grid, lat = lat.grid, pch = 16, cex = 0.3, col = "black")
sl.plot.points(pir, lon = points.lon, lat = points.lat, col = "red")
for (i in 1:length(points.lon)) {
  sl.plot.points(pir, lon = lon.grid[nn$nn.index$row[i],nn$nn.index$col[i]], lat = lat.grid[nn$nn.index$row[i],nn$nn.index$col[i]], col = "blue")
}
sl.plot.end(pir, do.close.device = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line