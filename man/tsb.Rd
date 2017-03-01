\name{tsb}
\alias{tsb}

\title{TSB (Teunter-Syntetos-Babai) method}

\description{TSB intermittent demand method with fixed or optimised parameters.}

\usage{
tsb(data,h=10,w=NULL,init=c("mean","naive"),
    cost=c("mar","msr","mae","mse"),
    init.opt=c(TRUE,FALSE),outplot=c(FALSE,TRUE),
    opt.on=c(FALSE,TRUE),na.rm=c(FALSE,TRUE))
}

\arguments{
  \item{data}{
    Intermittent demand time series.
}
  \item{h}{
    Forecast horizon.
}
  \item{w}{
    Smoothing parameters. If w == NULL then parameters are optimised. Otherwise first parameter is for demand and second for demand probability. 
}
  \item{init}{
    Initial values for demand and intervals. This can be: 1. c(z,x)  - Vector of two scalars, where first is initial demand and second is initial interval; 2. "naive" - Initial demand is first non-zero demand and initial demand probability is again the first one; 3. "mean"  - Same as "naive", but initial demand probability is the mean of all in sample probabilities.
}
  \item{cost}{
    Cost function used for optimisation: 1. "mar" - Mean Absolute Rate; 2. "msr" - Mean Squared Rate; 3. "mae" - Mean Absolute Error; 4. "mse" - Mean Squared Error.
}
  \item{init.opt}{
    If init.opt==TRUE then initial values are optimised.
}
  \item{outplot}{
    If TRUE a plot of the forecast is provided.
}
  \item{opt.on}{
    This is meant to use only by the optimisation function. When opt.on is TRUE then no checks on inputs are performed.
}
  \item{na.rm}{
    A logical value indicating whether NA values should be remove using the method.
}
}
\value{
\item{model}{Type of model fitted.}
\item{frc.in}{In-sample demand rate.}
\item{frc.out}{Out-of-sample demand rate.}
\item{weights}{Smoothing parameters for demand and demand probability.}
\item{initial}{Initialisation values for demand and demand probability smoothing.}
}
\references{
Optimisation of the method described in:
N. Kourentzes, 2014, On intermittent demand model optimisation and selection, International Journal of Production Economics, 156: 180-190. \url{http://dx.doi.org/10.1016/j.ijpe.2014.06.007}.

\url{http://kourentzes.com/forecasting/2014/06/11/on-intermittent-demand-model-optimisation-and-selection/}
}
\author{
Nikolaos Kourentzes
}
\seealso{
\code{\link{crost}}, \code{\link{sexsm}}, \code{\link{crost.ma}}.
}
\examples{
tsb(ts.data1,outplot=TRUE)
}
\keyword{ ~tsb }