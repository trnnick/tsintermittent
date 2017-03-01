\name{data.frc}
\alias{data.frc}

\title{Wrapper to forecasts data.frames with a single call}

\description{Wrapper to forecasts data.frames with a single call.}

\usage{
data.frc(data.in,method=c("crost","crost.ma","tsb","sexsm","imapa","auto"),...)
}

\arguments{
  \item{data.in}{
    Data frame with time series. This can also be a matrix or array with each column being a different time series.
}
  \item{method}{
    Which method to use for forecasting: "crost", "crost.ma", "tsb", "sexsm", "imapa", "auto". "auto" uses PKa classification to select automatically between Croston, SBA and SES.
}
  \item{...}{
    Additional inputs to pass to forecasting functions. See individual function documentation for options.
}
}
\value{
\item{frc.out}{Data frame containing forecasts for all time series.}
\item{out}{List with detailed output per series. To access individual outputs of the list use: sapply(out, get, x="element"), where "element" could be for example "frc.in".}
}
\references{
By default methods are optimised using the cost functions introduced by:
N. Kourentzes, 2014, On intermittent demand model optimisation and selection, International Journal of Production Economics, 156: 180-190. \url{http://dx.doi.org/10.1016/j.ijpe.2014.06.007}.

\url{http://kourentzes.com/forecasting/2014/06/11/on-intermittent-demand-model-optimisation-and-selection/}

The PK approximate classification is described in:
F. Petropoulos and N. Kourentzes, 2015, Journal of Operational Research Society. \url{http://www.palgrave-journals.com/jors/journal/v66/n6/full/jors201462a.html}.
\url{http://kourentzes.com/forecasting/2014/05/13/forecast-combinations-for-intermittent-demand/}
}
\author{
Nikolaos Kourentzes
}
\seealso{
\code{\link{crost}}, \code{\link{crost.ma}}, \code{\link{tsb}}, \code{\link{sexsm}}, \code{\link{imapa}}, \code{\link{idclass}}.
}
\examples{
data.frc(simID(10,30),method="crost",type="sba",h=5)$frc.out
}
\keyword{ ~data.frc }