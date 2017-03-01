\name{crost.decomp}
\alias{crost.decomp}

\title{Croston's decomposition}

\description{Apply Croston's decomposition on a time series.}

\usage{
crost.decomp(data,init=c("naive","mean"))
}

\arguments{
  \item{data}{
    Intermittent demand time series.
}
  \item{init}{
    Initial values for intervals. This can be: 1. x - Numerical value; 2. "naive" - Initial interval is the first interval from start of time series; 3. "mean" - Initial interval is the mean of all in sample intervals.
}
}
\value{
\item{demand}{Non-zero demand vector.}
\item{interval}{Intervals vector.}
}
\author{
Nikolaos Kourentzes
}
\seealso{
\code{\link{crost}}, \code{\link{crost.ma}}.
}
\examples{
crost.decomp(ts.data1)
}
\keyword{ ~croston ~crost ~crost.decomp}