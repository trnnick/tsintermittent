\name{imapa}
\alias{imapa}

\title{MAPA for intermittent demand data}

\description{MAPA for intermittent demand data with automatic model selection based on the PK classification.}

\usage{
imapa(data,h=10,w=NULL,minimumAL=1,maximumAL=NULL,comb=c("mean","median"),
      init.opt=c(TRUE,FALSE),paral=c(0,1,2),outplot=c(0,1,2,3,4),model.fit=NULL,
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
    Smoothing parameters. If w == NULL then parameters are optimised. If w is w single parameter then the same is used for smoothing both the demand and the intervals. If two parameters are provided then the second is used to smooth the intervals. SES is always optimised. 
}
  \item{minimumAL}{
    Lowest aggregation level to use. Default = 1.
}
  \item{maximumAL}{
    Highest aggregation level to use. Default = maximum interval.
}
  \item{comb}{
    Combination operator. One of "mean" or "median". Default is "mean".
}
  \item{init.opt}{
    If init.opt==TRUE then Croston and SBA initial values are optimised.
}
  \item{paral}{
    Use parallel processing. 0 = no; 1 = yes (requires initialised cluster); 2 = yes and initialise cluster. Default is 0.
}
  \item{outplot}{
    Optional plot: 0 = No plot; 1 = Time series and combined forecast; 2 = Time series and all aggregation level forecasts; 3 = Summary model selection plot; 4 = Detailed model selection plot.
}
  \item{model.fit}{
    Optional input with model types and parameters. This is the model.fit output from this function. If used it overrides other model settings.
}
  \item{na.rm}{
    A logical value indicating whether NA values should be remove using the method.
}
}
\value{
\item{frc.in}{In-sample demand rate.}
\item{frc.out}{Out-of-sample demand rate.}
\item{summary}{An array containing information for each aggregation level: AL - Aggregation level; n - Number of observations of aggregated series; p - Average inter-demand interval; cv2 - Coefficient of variation squared of non-zero demand; model - Selected model, where 1 is Croston, 2 is SBA and 3 is SES; use - If == 0 then this aggregation level is ignored because it contains less than 4 observations.}
\item{model.fit}{Parameters and initialisation values of fitted model in each aggregation level.}
}
\references{
Optimisation of the methods described in:
F. Petropoulos and N. Kourentzes, 2015, Forecast Combinations for Intermittent Demand, Journal of Operational Research Society. \url{https://link.springer.com/article/10.1057/jors.2014.62}.

\url{https://kourentzes.com/forecasting/2014/05/13/forecast-combinations-for-intermittent-demand/}
}
\note{Note on optimal model paramaters: This implementation of MAPA for intermittent demand data optimises smoothing parameters for all Croston's method, SBA and SES. Optimisation is based on:

N. Kourentzes, 2014, On intermittent demand model optimisation and selection, International Journal of Production Economics, 156: 180-190. \doi{10.1016/j.ijpe.2014.06.007}.

\url{https://kourentzes.com/forecasting/2014/06/11/on-intermittent-demand-model-optimisation-and-selection/}

Note on parallelisation: Option paral=2 incurs substantial overheads. For a single time series using no parallelisation seems to be as good. If imapa is to be applied on multiple series, then initialising the parallel cluster externally and using the option paral=1 is advised. 
}
\author{
Nikolaos Kourentzes
}
\seealso{
\code{\link{tsb}}, \code{\link{sexsm}}, \code{\link{crost}}, \code{\link{idclass}}.
}
\examples{
imapa(ts.data1,outplot=1)
}
\keyword{ ~imapa ~crost ~tsb ~ses ~sexsm}