\name{crost}
\alias{crost}

\title{Croston's method and variants}

\description{Croston's method and variants for intermittent demand series with fixed or optimised parameters.}

\usage{
crost(data,h=10,w=NULL,init=c("mean","naive"),nop=c(2,1),
      type=c("croston","sba","sbj"),cost=c("mar","msr","mae","mse"),
      init.opt=c(TRUE,FALSE),outplot=c(FALSE,TRUE),opt.on=c(FALSE,TRUE),
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
    Smoothing parameters. If w == NULL then parameters are optimised. If w is a single parameter then the same is used for smoothing both the demand and the intervals. If two parameters are provided then the second is used to smooth the intervals. 
}
  \item{init}{
    Initial values for demand and intervals. This can be: 1. c(z,x)  - Vector of two scalars, where first is initial demand and second is initial interval; 2. "naive" - Initial demand is first non-zero demand and initial interval is first interval; 3. "mean"  - Same as "naive", but initial interval is the mean of all in sample intervals.
}
  \item{nop}{
    Specifies the number of model parameters. Used only if they are optimised. 1. 1 - Demand and interval parameters are the same; 2. 2 - Different demand and interval parameters.
}
  \item{type}{
    Croston's method variant: 1. "croston" Croston's method; 2. "sba" Syntetos-Boylan approximation; 3. "sbj" Shale-Boylan-Johnston.
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
\item{weights}{Smoothing parameters for demand and interval.}
\item{initial}{Initialisation values for demand and interval smoothing.}
\item{component}{List of c.in and c.out containing the non-zero demand and interval vectors for in- and out-of-sample respectively. Third element is the coefficient used to scale demand rate for sba and sbj.}
}
\references{
Optimisation of the methods described in:
N. Kourentzes, 2014, On intermittent demand model optimisation and selection, International Journal of Production Economics, 156: 180-190. \url{http://dx.doi.org/10.1016/j.ijpe.2014.06.007}.

\url{http://kourentzes.com/forecasting/2014/06/11/on-intermittent-demand-model-optimisation-and-selection/}
}
\author{
Nikolaos Kourentzes
}
\seealso{
\code{\link{tsb}}, \code{\link{sexsm}}, \code{\link{crost.ma}}.
}
\examples{
crost(ts.data1,outplot=TRUE)
}
\keyword{ ~croston ~crost }