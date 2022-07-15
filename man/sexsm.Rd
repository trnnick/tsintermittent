\name{sexsm}
\alias{sexsm}

\title{Simple exponential smoothing}

\description{Simple exponential smoothing with fixed or optimised parameters.}

\usage{
sexsm(data,h=10,w=NULL,init=c("mean","naive"),
      cost=c("mar","msr","mae","mse"),init.opt=c(TRUE,FALSE),
      outplot=c(FALSE,TRUE),opt.on=c(FALSE,TRUE),
      na.rm=c(FALSE,TRUE))
}

\arguments{
  \item{data}{
    Intermittent demand time series.
}
  \item{h}{
    Forecast horizon.
}
  \item{w}{
    Smoothing parameter. If w == NULL then parameter is optimised.
}
  \item{init}{
    Initial values for demand and intervals. This can be: 1. x - Numeric value for the initial level; 2. "naive" - Initial value is a naive forecast; 3. "mean"  - Initial value is equal to the average of data.
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
\item{frc.in}{In-sample demand.}
\item{frc.out}{Out-of-sample demand.}
\item{alpha}{Smoothing parameter.}
\item{initial}{Initialisation value.}
}
\references{
Optimisation of the method described in:
N. Kourentzes, 2014, On intermittent demand model optimisation and selection, International Journal of Production Economics, 156: 180-190. \doi{10.1016/j.ijpe.2014.06.007}.

\url{https://kourentzes.com/forecasting/2014/06/11/on-intermittent-demand-model-optimisation-and-selection/}
}
\author{
Nikolaos Kourentzes
}
\seealso{
\code{\link{crost}}, \code{\link{tsb}}, \code{\link{crost.ma}}.
}
\examples{
sexsm(ts.data1,outplot=TRUE)
}
\keyword{ ~sexsm }