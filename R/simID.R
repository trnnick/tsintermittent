#' Simulator for intermittent demand series
#'
#' @description Simulate intermittent demand time series, where demand occurrence
#' follows a Bernoulli distribution and the non-zero demand a negative binomial
#' distribution.
#'
#' @param n Number of time series to be generated.
#' @param obs Number of observations of each series.
#' @param idi Average inter-demand interval of each series.
#' @param cv2 Squared coefficient of variation of the non-zero demand.
#' @param level Mean level of the non-zero demand. If \code{NULL}, then a random level
#'   in [10,100] is selected for each series.
#'
#' @return A data frame with each generated series as a column.
#'
#' @references
#' Petropoulos, F., Makridakis, S., Assimakopoulos, V., and Nikolopoulos, K. (2014).
#' 'Horses for courses' in demand forecasting. \emph{European Journal of Operational
#' Research}, 237(1), 152--163.
#'
#' @author Fotios Petropoulos
#'
#' @seealso \code{\link{idclass}}, \code{\link{data.frc}}.
#'
#' @examples
#' # 100 series of 60 observations
#' dataset <- simID(100, 60, idi=1.15, cv2=0.3)
#' dim(dataset)
#'
#' # More intermittent series with a fixed level
#' dataset <- simID(10, 60, idi=3, cv2=0.5, level=20)
#' plot(dataset[,1], type="h")
#'
#' @export simID
simID <- function(n=1,obs=60,idi=2,cv2=0.5,level=NULL){

  series <- array(NA,c(obs,n))

  for (tsi in 1:n){
    if (is.null(level)){
      m <- runif(1,9,99)
    } else {
      m <- level - 1
    }

    if (cv2!=0){
      p <- (m/(cv2*((m+1)^2)))
      r <- m*p/(1-p)
      x <- rbinom(obs,1,1/idi) * (rnbinom(obs,r,p)+1) # rbern(obs,1/idi) replaced by binomial
    } else {
      x <- rbinom(obs,1,1/idi) * round(m+1) # rbern(obs,1/idi) replaced by binomial
    }

    series[,tsi] <- x
  }

  # Convert series to data.frame
  colnames(series) <- paste("ts",1:n)
  series <- data.frame(series)

  return(series)

}
