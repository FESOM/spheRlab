\name{sl.ens.gcdist.rmse}
\alias{sl.ens.gcdist.rmse}
\title{
Calculate root mean square great-circle distance error of an ensemble
}
\description{
  This function can be used to calculate the root mean square of the great-circle distance errors of a trajectory ensemble, either compared to an observed trajectory or to each one of the ensemble members in a perfect-model approach.
}
\usage{
sl.ens.gcdist.rmse(ensm.in, t.ind = 1:length(ensm.in[1,1,]), obs = NULL, mode = "pm", Rsphere = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ensm.in}{a numeric array containing the trajectory positions (in lon-lat coordinates), with shape [N.mem, 2, N.time], with the number of members \code{N.mem} and the number of time steps \code{N.time}.}
  \item{t.ind}{a numeric vector of maximal length N.time, containing the indices of the time steps that shall be considered. All time steps are used by default, i.e. \code{t.inds = 1:N.time}.}
  \item{obs}{a numeric array of shape [2,N.time] which contains the coordinates of an observed trajectory. Must be provided if \code{mode="obs"} is used.}
  \item{mode}{a string, either 'pm' (perfect model) or 'obs' (observation), defining the type of RMSE that will be computed. See section 'Details' for more information.}
  \item{Rsphere}{ a numerical scalar defining the radius of the sphere for which the great-circle distance of the positions is calculated.}
}
\details{
For \code{mode='pm'}, the root mean square error is calculated using each ensemble member as the 'truth' once (see reference for further information). Otherwise, the observation is used to determine the position errors. 
}
\value{
Returns a numeric vector of length \code{length(t.ind)} with a time series of the RMS distance error. 
}
\references{
Collins, M. (2003), Climate Predictability on Seasonal to Decadal Time Scales: The Initial Value Problem, Climate Dyamics, 19, 
}
\author{
Simon Reifenberg
}

\seealso{
  \code{\link{sl.gc.dist}}
}