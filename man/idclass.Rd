\name{idclass}
\alias{idclass}

\title{Time series categorisation for intermittent demand}

\description{Time series categorisation for intermittent demand data.}

\usage{
idclass(data,type=c("PKa","SBC","KHa","KH","PK"),a.in=NULL,
        outplot=c("summary","detail","none"),plot.focus=NULL)
}

\arguments{
  \item{data}{
    Time series dataset. Each column is a series. Alternatively this can be a single series.
}
  \item{type}{
    Type of categorisation: 1. "SBC" - Syntetos Boylan Croston; 2. "KH"  - Kostenko Hyndman (exact*); 3. "KHa" - Kostenko Hyndman (approximate); 4. "PK"  - Petropoulos Kourentzes (exact*); 5. "PKa" - Petropoulos Kourentzes (approximate). *These are computationally expensive, as SBA is optimised for each time series. 
}
  \item{a.in}{
    Vector of SBA demand interval smoothing parameters. This must be same length as number of series. This is used for categorisations "KH" and "PK". If a.in == NULL then the parameters are calculated internally using MAR as a cost function.
}
  \item{outplot}{
    Plot results of categorisation: 1. "summary" - simlified plot that reports number of series in each class and cut-off points; 2. "detail" - scatterplot between average interdemand interval (p) and squared coefficient of variation of non-zero demand (CV^2). Series that are categorised for SBA or SES are plotted in shaded areas; 3. "none" - do not produce plot.
}
  \item{plot.focus}{
    Only relevant to outplot == "detail". Can be used to specify the maximum p and CV^2 to plot, so that the scatterplot can be focused on the separation area between the categories. Use vector of two elements. First one is max p and second one is max CV^2. Example: plot.focus=c(1.5,1.5). If NULL then maximums are defined from the dataset. 
}
}
\value{
\item{idx.croston}{Index of series that are categorised under Croston.}
\item{idx.sba}{Index of series that are categorised under SBA.}
\item{idx.ses}{Index of series that are categorised under SES. Provided only for "PK" and "PKa" types.}
\item{cv2}{Coefficient of variation squared of non-zero demand.}
\item{p}{Inter-demand interval.}
\item{summary}{Summary of number of series under each category.}
}
\references{
Classification schemes described in:
F. Petropoulos and N. Kourentzes, 2015, Journal of Operational Research Society. \url{http://www.palgrave-journals.com/jors/journal/v66/n6/full/jors201462a.html}.
\url{http://kourentzes.com/forecasting/2014/05/13/forecast-combinations-for-intermittent-demand/}

Optimisation of the methods described in:
N. Kourentzes, 2014, On intermittent demand model optimisation and selection, International Journal of Production Economics, 156: 180-190. \url{http://dx.doi.org/10.1016/j.ijpe.2014.06.007}.

\url{http://kourentzes.com/forecasting/2014/06/11/on-intermittent-demand-model-optimisation-and-selection/}
}
\author{
Nikolaos Kourentzes
}
\seealso{
\code{\link{crost}}, \code{\link{tsb}}, \code{\link{imapa}}.
}
\examples{
# Create/load some data. Each column is a time series
dataset <- simID(100,60,idi=1.15,cv2=0.3)
idclass(dataset)
}
\keyword{ ~idclass ~crost ~croston ~tsb }