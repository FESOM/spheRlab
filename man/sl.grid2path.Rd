\name{sl.grid2path}
\alias{sl.grid2path}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Path from Gridded Field
}
\description{
Extract values along a path from a time-varying or constant curvilinear gridded data field. Interpolation is nearest-neighbour in space and linear in time.
}
\usage{
sl.grid2path(dat.time = NULL, dat = NULL, dat.files = NULL, dat.timeind = rep(1,length(dat.time)), dat.hastimedim = TRUE, dat.lon = NULL, dat.lat = NULL, traj.time = NULL, traj.lon = NULL, traj.lat = NULL, vars = NULL, max.dist = Inf, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat.time}{
  a numeric vector of length \code{T} providing the time axis of the gridded field. Must be strictly monotonously increasing. Required only if the gridded field has a time dimension.
  }
  \item{dat}{
  a 3-dimensional (\code{X,Y,T}) or 2-dimensional (\code{X,Y}) numeric array providing the data values of the curvilinear data field. If \code{dat=NULL} (default), \code{dat.files} must be provided instead.
  }
  \item{dat.files}{
  a character vector of length \code{T} providing the file names (including paths) of one or more NetCDF files holding the 3-dimensional (\code{X,Y,T}) or 2-dimensional (\code{X,Y}) data values of the curvilinear data field. Each element of \code{dat.files} corresponds to the respective entry of \code{dat.time}. This implies that, if individual files contain multiple timesteps, these file names (including paths) must be provided repeatedly. In this case, \code{dat.timeind} must be provided explicitly. \code{dat.files} is used only if \code{dat=NULL}.
  }
  \item{dat.timeind}{
  an integer vector of length \code{T} providing the time indices corresponding to \code{dat.files}. Each element of \code{dat.timeind} corresponds to the respective entry of \code{dat.files} and \code{dat.time}. If multiple timesteps are contained in one of the files (which is thus provided repeatedly in \code{dat.files}), \code{dat.timeind} is required to ensure a correct temporal mapping. Default is \code{dat.timeind=rep(1,length(dat.time))}, which assumes that each time step is in a separate file. \code{dat.timeind} (and \code{dat.files}) are used only if \code{dat=NULL}.
  }
  \item{dat.hastimedim}{
  a logical value specifying whether the gridded data has a time dimension at all. Default is \code{TRUE}.
  }
  \item{dat.lon}{
  a numerical matrix of size \code{X,Y} providing the longitudes of the curvilinear grid.
  }
  \item{dat.lat}{
  a numerical matrix of size \code{X,Y} providing the latitudes of the curvilinear grid.
  }
  \item{traj.time}{
  a numeric vector of length \code{N} providing the time axis of the path (can but does not need to be an actual trajectory). Must be strictly monotonously increasing. Required only if the gridded field has a time dimension.
  }
  \item{traj.lon}{
  a numeric vector of length \code{N} providing the longitudes of the path.
  }
  \item{traj.lat}{
  a numeric vector of length \code{N} providing the latitudes of the path.
  }
  \item{vars}{
  a character vector specifying the variable names. If the gridded data is read from NetCDF files, these must be identical with the names of the desired variables in the NetCDF files. If more than one variable are specified, the function output will include more than one path/trajectory.
  }
  \item{max.dist}{
  a numeric scalar providing the maximum distance (in kilometers) to the nearest neighbour beyond which the assigned value will be set \code{NA}. Default is \code{Inf}, implying that the nearest-neighbour value is used irrespective of the distance. Setting a finite value for \code{max.dist} can be useful to avoid values when the path is clearly outside the region covered by the gridded data.
  }
  \item{verbose}{
  a logical value specifying whether some information is provided during the execution via print statements. Default is \code{FALSE}.
  }
}
\details{
For each data point provided by \code{traj.time}, \code{traj.lon}, and \code{traj.lat}, the algorithm first checks whether the previous nearest neighbour is still the nearest neighbour by comparing only against the adjacent grid points. If it's not, the nearest neighbour search is done using \code{\link{sl.findnn.curvilin}}.

New data from the gridded field is read only if the time (for temporal data) of the path/trajectory surpasses a time step of the gridded data, or if the nearest neighbour has changed. If both is not the case, the previously-read data is used and, if applicable, only the weight for the linear time interpolation is adjusted.
}
\value{
  a list with three elements:
  \item{data}{
  a list with one element for each of the variables specified in the argument \code{vars}; each of these is a numeric vector of length \code{N} containing the data values along the path/trajectory for the respective variable.
  }
  \item{nn.index}{
  an integer matrix of size \code{Nx2} providing the indices of the respective nearest neighbour from which each data value was fetched.
  }
  \item{nn.distance}{
  a numeric vector of length \code{N} providing the distance from the respective nearest neighbour from which each data value was fetched.
  }
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Helge Goessling
}
\note{
To extensions would be nice:
(i) Complement the function such that not only nearest-neighbour but more sophisticated spatial interpolation (e.g., bilinear) is supported.
(ii) Support of unstructured grids.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sl.findnn.curvilin}}
}
\examples{
# generate an example curvilinear grid; first just lon-lat, then rotate to make it more general
lon.seq = seq(-60,60,2)
lat.seq = seq(-30,30,2)
Nx = length(lon.seq)
Ny = length(lat.seq)
lon.0 = matrix(rep(lon.seq,Ny),nrow=Nx)
lat.0 = matrix(rep(lat.seq,each=Nx),nrow=Nx)
abg = sl.lonlatrot2abg(c(0,0,30))
rot.lonlat = sl.rot(lon = lon.0, lat = lat.0, alpha = abg[1], beta = abg[2], gamma = abg[3])
lon = matrix(rot.lonlat$lon,nrow=Nx)
lat = matrix(rot.lonlat$lat,nrow=Nx)

# generate corresponding example data; first just a static field, then add time dimension with simple modulation
# NOTE: instead of providing an data array directly as argument, data can also be read effectively (only along the path)
# from NetCDF files as specified by the arguments 'dat.files' and 'dat.timeind'
dat.0 = matrix(exp(-((sl.gc.dist(lon = c(180,lon), lat = c(85,lat), sequential = FALSE)/0.4)^2)),nrow=Nx)
dat.time = seq(1,100)
Nt = length(dat.time)
dat = array(dim=c(Nx,Ny,Nt))
for (ti in 1:Nt) {
  dat[,,ti] = dat.0 + dat.time[ti]/100
}

# generate a path, including a time dimension that purposely partly exceeds the spatial as well as temporal data range
traj.time = seq(20,120,0.2)
traj.N = length(traj.time)
traj.lon = seq(-180,180,360/(traj.N-1))
traj.lat = cos(traj.time/5)*20 + 65

# now apply the function to extract the data along the path
g2p = sl.grid2path(dat.time = dat.time, dat = dat, dat.lon = lon, dat.lat = lat, traj.time = traj.time, traj.lat = traj.lat, traj.lon = traj.lon)
g2p.maxdist = sl.grid2path(dat.time = dat.time, dat = dat, dat.lon = lon, dat.lat = lat, traj.time = traj.time, traj.lat = traj.lat, traj.lon = traj.lon, max.dist = 500)

# visualize the grid, path, and extracted data
# note how both 'g2p' and 'g2p.maxdist' have NA values at the end, because th temporal range of 'dat' is exceeded,
# and how 'g2p.maxdist' additionally has NA values inbetween where the trajectory is too far outside the range of the grid
par(mfrow=c(2,1))
# map
pir = sl.plot.init(projection = "polar", do.init.device = FALSE)
sl.plot.naturalearth(pir, what = "land", resolution = "coarse", fill.col = "grey")
sl.plot.lonlatgrid(pir, col = "black", pole.hole = TRUE)
sl.plot.points(pir, lon = lon, lat = lat, pch = 16, cex = 0.3, col = "blue")
sl.plot.lines(pir, lon = traj.lon, lat = traj.lat, col = "red", lwd = 1.5)
sl.plot.end(pir, do.close.device = FALSE)
# timeseries
par(mar = c(5, 4, 4, 2) + 0.1)
xlim = range(c(dat.time,traj.time))
ylim = range(g2p$data[[1]], na.rm = TRUE)
plot(NA, xlim = xlim, ylim = ylim, xlab = "time", ylab = "values along path/trajectory")
rect(xleft = min(dat.time), xright = max(dat.time), ybottom = extendrange(ylim, f = 2)[1],
     ytop = extendrange(ylim, f = 2)[2], col = rgb(0,0,1,0.5), border = NA)
rect(xleft = min(traj.time), xright = max(traj.time), ybottom = extendrange(ylim, f = 2)[1],
     ytop = extendrange(ylim, f = 2)[2], col = rgb(1,0,0,0.5), border = NA)
lines(traj.time, g2p$data[[1]])
lines(traj.time, g2p.maxdist$data[[1]], lty = "dashed", col = "grey")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line