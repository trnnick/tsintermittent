\name{crost.ma}
\alias{crost.ma}

\title{Moving average with Croston's method decomposition}

\description{Moving average with Croston's method decomposition for intermittent demand series with fixed or optimised parameters.}

\usage{
crost.ma(data,h=10,w=NULL,nop=c(2,1),type=c("croston","sba","sbj"),
         cost=c("mar","msr","mae","mse"),outplot=c(FALSE,TRUE),
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
    Moving average order. If w == NULL then moving average orders are optimised. If w is a single value then the same order is used for smoothing both the demand and the intervals. If two values are provided then the second is used to smooth the intervals. 
}
  \item{nop}{
    Specifies the number of model parameters. Used only if they are optimised. 1. 1 - Demand and interval moving average order are the same; 2. 2 - Different demand and interval orders.
}
  \item{type}{
    Croston's method variant: 1. "croston" Croston's method; 2. "sba" Syntetos-Boylan approximation; 3. "sbj" Shale-Boylan-Johnston.
}
  \item{cost}{
    Cost function used for optimisation: 1. "mar" - Mean Absolute Rate; 2. "msr" - Mean Squared Rate; 3. "mae" - Mean Absolute Error; 4. "mse" - Mean Squared Error.
}
  \item{outplot}{
    If TRUE a plot of the forecast is provided.
}
  \item{na.rm}{
    A logical value indicating whether NA values should be remove using the method.
}
}
\value{
\item{model}{Type of model fitted.}
\item{frc.in}{In-sample demand rate.}
\item{frc.out}{Out-of-sample demand rate.}
\item{order}{Moving averages orders for demand and interval.}
\item{component}{List of c.in and c.out containing the non-zero demand and interval vectors for in- and out-of-sample respectively. Third element is the coefficient used to scale demand rate for sba and sbj.}
}
\references{
Optimisation cost functions described in:
N. Kourentzes, 2014, On intermittent demand model optimisation and selection, International Journal of Production Economics, 156: 180-190. \doi{10.1016/j.ijpe.2014.06.007}.

\url{https://kourentzes.com/forecasting/2014/06/11/on-intermittent-demand-model-optimisation-and-selection/}
}
\author{
Nikolaos Kourentzes
}
\seealso{
\code{\link{crost}}, \code{\link{tsb}}, \code{\link{sexsm}}.
}
\examples{
crost.ma(ts.data1,outplot=TRUE)
}
\keyword{ ~croston ~crost.ma ~moving average }