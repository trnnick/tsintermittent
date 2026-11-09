#' Croston decomposition
#'
#' @description Decompose an intermittent demand time series into the non-zero demand
#' and the intervals between demands, as used by Croston's method.
#'
#' @param data Intermittent demand time series. This can be a vector, a \code{ts}
#'   object, or a data frame, in which case only the first column is used.
#' @param init Initial value for the intervals. This can be:
#'   \itemize{
#'   \item a numeric value;
#'   \item \code{"naive"}: the first interval is the number of periods from the start
#'   of the series to the first demand;
#'   \item \code{"mean"}: the first interval is the mean of all in-sample intervals.
#'   }
#'
#' @return A list containing:
#' \item{demand}{Non-zero demand vector.}
#' \item{interval}{Intervals vector.}
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{crost}}.
#'
#' @examples
#' crost.decomp(ts.data1)
#'
#' # The first interval replaced by the mean interval
#' crost.decomp(ts.data1, init="mean")$interval
#'
#' @export crost.decomp
crost.decomp <- function(data,init=c("naive","mean")){

  # Defaults
  if (!is.numeric(init)){
    init <- id.match(init,c("naive","mean"),"init")
  }

  if (isa(data,"data.frame")){
    if (ncol(data)>1){
      warning("Data frame with more than one columns. Using only first one.")
    }
    data <- data[[1]]
  }

  # Check number of non-zero values - need to have at least one
  if (sum(data!=0,na.rm=TRUE)<1){
    stop("Need to have at least one non-zero demand to decompose the time series.")
  }

  # Croston decomposition
  nzd <- which(data != 0)               # Find location on non-zero demand
  k <- length(nzd)
  z <- data[nzd]                        # Demand
  if (k>1){
    x <- c(nzd[1],nzd[2:k]-nzd[1:(k-1)])  # Intervals
  } else {
    x <- nzd
  }

  # Initialise intervals
  if (!(is.numeric(init))){
    if (init=="mean"){
      x[1] <- mean(x)
    }
  } else {
    x[1] <- init[1]
  }

  return(list(demand=z,interval=x))

}
