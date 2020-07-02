\name{sl.tracer.calculate.trajectories}
\alias{sl.tracer.calculate.trajectories}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate kinematic trajectories
}
\description{
A suite of three different integration methods for kinematic trajectory calculations on unstructured triangular meshes.
}
\usage{
sl.tracer.calculate.trajectories(method = "Petterssen", U, x.ini, t.ini = 0, grid, i.neighs, tri.cont.ini = NULL, abort.if = NULL, tgr = NULL, dt.def = 1, T.end = 10, maxiter = 4, redfac = 2,  cart_geo= "geo", patch.level = 10, Rsphere = 6371)
}
%- maybe also 'usage' for other objects documented here.
\arguments{ %U, x.ini, t.ini = 0, grid, i.neighs, tri.cont.ini = NULL, abort.if = NULL, tgr = NULL, dt.def = 1, T.end = 10,                             maxiter = 4, redfac = 2,  cart_geo= "geo", patch.level = 1
  \item{method}{a string defining the integration scheme, either 'Petterssen', 'Euler' or 'Pansy'. The latter is currently only available for Cartesian coordinates.}
  \item{U}{a numerical array (dim: \code{[2, N.nodes, N.time]}), where \code{N.time} is the number of available time steps and \code{N.nodes} the number of nodes defining the grid. \code{U[1,,]} contains the velocity component in zonal (or x-) direction, and \code{U[2,,]} contains the velocity component in meridional (or y-) direction. }
  \item{x.ini}{a numerical array (dim: \code{[2, N.ini]}) containing the initial positions. Longitudes (or x-values) are therefore given by \code{x.ini[1,]} and latitudes (or y-values) by \code{x.ini[2,]}. \code{N.ini} is the number of initial positions.}
  \item{t.ini}{a numerical scalar, the time for which the initial positions \code{x.ini} are valid. Default is \code{t.ini=0}.}
  \item{grid}{a list containing at least the entries \code{lon}, \code{lat} and \code{elem}. The former two are numerical vectors containing the coordinates of the nodes (with length \code{N.nodes}), the latter is a numerical array (dim: \code{[N.elem,3]}) defining the FEM grid by storing the indices of the node coordinates in the rows. }
  \item{i.neighs}{a list of length \code{N.elem} which contains all elements (indices) that share at least one node with the given element, i.e. \code{i.neighs[[17]]} contains the indices of all neighboring elements for the element with index 17. If not provided, this list will be created during runtime.}
  \item{tri.cont.ini}{a numerical vector of length N.ini, containing the triangles (indices) that contain the initial positions. If not provided, it will be determined during runtime, which can be quite exhaustive for large numbers of elements.}
  \item{abort.if}{<documentation pending>}
  \item{tgr}{a numerical vector, the time grid of the input data. If not speficied, it is set to \code{0:(N.time-1)}. The other time-related variables must match the units of this vector.}
  \item{dt.def}{a numeric scalar defining the default time step size (in units of \code{tgr}).}
  \item{T.end}{a numeric scalar defining the maximal length of the time integration (in units of \code{tgr}).}
  \item{maxiter}{an integer scalar defining the maximum number of time step adjustments per time step}
  \item{qfrac}{a scalar number (greater than 1) by which the time step is divided during the adjustment}
  \item{cart_geo}{a string, either 'cart' or 'geo', defining if the grid shall be treated as geographic coordinates ('geo') or Cartesian coordinates ('cart').}
  \item{geo.gc_rad}{a string, either 'gc' or 'rad', defining how the displacement is calculated for geographical coordinates. Either as great-circle distance ('gc') or approximating with small angles in northward and eastward direction ('rad').}
  \item{patch.level}{an integer number defining a search radius for triangle tracking, I recommend keeping the default.}
  \item{Rsphere}{a numerical scalar, defining the radius of the sphere for \code{cart_geo='geo'}.}
}
\details{
This function contains three different methods for trajectory calculations on unstructured triangular meshes for two-dimensional velocity fields: the well-known (first-order) explicit Euler method, the second-order Petterssen scheme, and a piece-wise analytical approach, Pansy. The latter is yet only available for Cartesian coordinates, but will be refurbished soon. All methods operate with an adaptive time step, as this both ensures efficiency of the algorithm and reduces errors. 

There are a lot of input parameters, but most of them can usually be ignored. For large numbers of elements within the grid, I recommend calculating \code{i.neighs} and \code{tri.cont.ini} once in advance for repeated use. As a rule of thumb, the default time step size should roughly equal (or be lower than) the ratio of a common element circumradius and a representative value for speed.

The time grid is a numerical vector here, not a 'time' object. Therefore you need to ensure manually that the input time grid \code{tgr} (and eventually the radius \code{Rsphere}) matches the units of the velocity vectors. If \code{U} is for instance given in meter per seconds, it is one possible solution to
\itemize{
  \item{}{set \code{Rsphere} to a value in [m], for instance \code{6371e3}}
  \item{}{set \code{tgr} to values in [s], for instance \code{0:10*86400} for daily velocities}
  \item{}{set \code{dt.def} to a value in [s], for instance \code{0.5*86400} for half a day.}
}
This also holds for \code{T.end} and \code{t.ini}. 

}
\value{
  A list containing
  \item{pos}{a numerical array (dim: \code{[N.ini, 3, N.time.out.max]}) containing the trajectories (\code{[,1:2,]}) and the elements of the given position (\code{[,3,]}) for the output time grid of length \code{N.time.out.max}, which is set by \code{maxiter}, \code{dt.def} and \code{qfrac}.}
  \item{time}{a numerical array (dim: \code{[N.ini, N.time.out.max]}) containing the individual output time grids for each trajectory. Due to the adaptive time step sizes, these do not necessarily coincide.} 
  \item{flag}{a vector of length N.ini which eventually contains flags for each trajectory under certain conditions (<will be revised soon>). If all entries are \code{NA}, it is very likely that the trajectories meet your expectations.}
  
}
\references{
 <work in progress>
}
\author{
Simon Reifenberg
}

\examples{
#################################################################
############### Example for Cartesian coordinates ###############
#################################################################

# generate and load example data (exchange filepath 'fp' at will)
fp = "~/"
sl.tracer.generate.testcase(filepath = fp)
load(paste0(fp, "sl.tracer.example.data.RData"))        # contains U, x.ini, grid, i.neighs, tri.cont.ini

##### TRAJECTORY COMPUTATION ####
dt.def  = 1     # set default time step size, for playing around I recommend values between 0.1 and 4

# only 'grid', 'U', 'x.ini' are used from the example data to keep the function call comparably tidy here
# -> T.end, t.ini, tgr, maxiter, qfrac are set to default, tri.cont.ini and i.neights are calculated during runtime
trajPe <- sl.tracer.calculate.trajectories(U, x.ini, grid=grid, cart_geo="cart", dt.def = dt.def, method="Petterssen")
trajEu <- sl.tracer.calculate.trajectories(U, x.ini, grid=grid, cart_geo="cart", dt.def = dt.def, method="Euler")
trajPa <- sl.tracer.calculate.trajectories(U, x.ini, grid=grid, cart_geo="cart", dt.def = dt.def, method="Pansy")

##### PLOT RESULTS #####
#(the analytical solution would be on concentric circles, black dots mark initial positions)
par(pty="s", mfrow = c(1,3))
# Petterssen
pir <- sl.plot.init("lonlat", lonlat.lonrange = c(-7,7), lonlat.latrange = c(-7,7), do.init.device = F)
sl.plot.elem(pir, grid$lon, grid$lat, grid$elem, col.border = "grey", fill = F, lwd = .5)
for (itraj in 1:10){sl.plot.lines(pir, lon = trajPe$pos[itraj,1,], lat = trajPe$pos[itraj,2,], col = "purple", lwd  = 1.5)}
sl.plot.points(pir, lon = x.ini[1,], lat = x.ini[2,], col = "black", pch = 19, cex = .7) # init. positions
sl.plot.end(pir, do.close.device = F); mtext("Petterssen", col = "gray40", cex = 1.2)

# Euler
pir <- sl.plot.init("lonlat", lonlat.lonrange = c(-7,7), lonlat.latrange = c(-7,7), do.init.device = F)
sl.plot.elem(pir, grid$lon, grid$lat, grid$elem, col.border = "grey", fill = F, lwd = .5)
for (itraj in 1:10){sl.plot.lines(pir, lon = trajEu$pos[itraj,1,], lat = trajEu$pos[itraj,2,], col = "steelblue", lwd  = 1.5)}
sl.plot.points(pir, lon = x.ini[1,], lat = x.ini[2,], col = "black", pch = 19, cex = .7) # init. positions
sl.plot.end(pir, do.close.device = F); mtext("Euler", col = "gray40", cex = 1.2)

# Pansy
pir <- sl.plot.init("lonlat", lonlat.lonrange = c(-7,7), lonlat.latrange = c(-7,7), do.init.device = F)
sl.plot.elem(pir, grid$lon, grid$lat, grid$elem, col.border = "grey", fill = F, lwd = .5)
for (itraj in 1:10){sl.plot.lines(pir, lon = trajPa$pos[itraj,1,], lat = trajPa$pos[itraj,2,], col = "orange", lwd  = 1.5)}
sl.plot.points(pir, lon = x.ini[1,], lat = x.ini[2,], col = "black", pch = 19, cex = .7) # init. positions
mtext("Pansy", col = "gray40", cex = 1.2); sl.plot.end(pir, do.close.device = F)
}