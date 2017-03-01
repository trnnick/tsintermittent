\name{simID}
\alias{simID}

\title{Simulator for Intermittent Demand Series}

\description{Simulator of Intermittent Demand Series.}

\usage{
simID(n=1, obs=60, idi=2, cv2=0.5, level=NULL)
}

\arguments{
  \item{n}{
    Number of time series to be generated.
}
  \item{obs}{
    Number of observation of each series.
}
  \item{idi}{
    Average intermittent demand interval of each series.
}
  \item{cv2}{
    Squared coefficient of variation of the non-zero demands.
}
  \item{level}{
    Mean level of the non-zero demands. If NULL, then a random level in [10,100] is selected.
}
}
\value{
\item{series}{A data matrix containing all the generated series.}
}
\references{
This simulator assumes that non-zero demand arrivals follow a bernoulli distribution and the non-zero demands a negative binomial distribution.
Petropoulos F., Makridakis S., Assimakopoulos V. & Nikolopoulos K. (2014) "'Horses for Courses' in demand forecasting", European Journal of Operational Research, Vol. 237, No. 1, pp. 152-163
}
\author{
Fotios Petropoulos
}
\seealso{
\code{\link{crost}}, \code{\link{tsb}}, \code{\link{idclass}}.
}
\examples{
dataset <- t(simID(100,60,idi=1.15,cv2=0.3))
}
\keyword{ ~simID }