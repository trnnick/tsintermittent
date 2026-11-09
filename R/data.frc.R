#' Forecast multiple intermittent demand series
#'
#' @description Forecast all columns of a data frame or matrix of intermittent demand
#' series with a single call.
#'
#' @param data.in Data frame with time series, one per column. This can also be a
#'   matrix or array with each column being a different time series.
#' @param method Forecasting method: \code{"crost"}, \code{"crost.ma"}, \code{"tsb"},
#'   \code{"sexsm"}, \code{"imapa"}, or \code{"auto"}. The latter selects between
#'   Croston's method, SBA, and SES for each series, using the PKa classification of
#'   \code{\link{idclass}}.
#' @param ... Additional inputs passed to the forecasting functions, for example
#'   \code{h}, \code{cost}, \code{probs}, or \code{outplot}. See the documentation of
#'   the individual functions for the options. With \code{method="auto"}, only the
#'   inputs accepted by both \code{\link{crost}} and \code{\link{sexsm}} are used,
#'   except \code{nop}, which is used only by \code{\link{crost}}.
#'
#' @details With \code{method="auto"}, series that cannot be classified, because they
#'   have fewer than two non-zero values, are forecasted with SES.
#'
#' @return A list containing:
#' \item{frc.out}{Data frame containing the forecasts of all time series.}
#' \item{out}{List with the output of the forecasting function for each series. To
#'   access an element of all outputs use, for example,
#'   \code{lapply(out, "[[", "frc.in")}.}
#' \item{lt.out}{Only when \code{probs} is provided. Data frame containing the
#'   quantiles of the demand over the lead time of all time series. The safety stock and
#'   the other lead time results of each series are in its \code{lt} element of
#'   \code{out}.}
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{crost}}, \code{\link{tsb}}, \code{\link{sexsm}},
#'   \code{\link{crost.ma}}, \code{\link{imapa}}, \code{\link{idclass}}.
#'
#' @examples
#' set.seed(1)
#' dataset <- simID(5, 60, idi=1.5, cv2=0.5)
#'
#' # SBA for all series
#' fit <- data.frc(dataset, method="crost", type="sba", h=6)
#' fit$frc.out
#'
#' # Smoothing parameters of all series
#' sapply(fit$out, "[[", "weights")
#'
#' # Automatic selection between Croston, SBA, and SES
#' data.frc(dataset, method="auto", h=6)$frc.out
#'
#' # Quantiles of the demand over a lead time of 3 periods
#' data.frc(dataset, method="tsb", h=3, probs=c(0.9,0.95))$lt.out
#'
#' @export data.frc
data.frc <- function(data.in,method=c("crost","crost.ma","tsb","sexsm","imapa","auto"),...){

  # Get defaults
  method <- id.match(method,c("crost","crost.ma","tsb","sexsm","imapa","auto"),"method")
  if (is.null(dim(data.in))){
    data.in <- matrix(data.in,ncol=1)
  }
  data.names <- colnames(data.in)
  if (is.null(data.names)){
    data.names <- paste0("ts",1:ncol(data.in))
  }
  N <- ncol(data.in)
  dots <- list(...)

  # Fit models and produce forecasts
  if (method != "auto"){
    fun <- switch(method,"crost"=crost,"crost.ma"=crost.ma,"tsb"=tsb,"sexsm"=sexsm,"imapa"=imapa)
    out <- lapply(1:N,function(i){do.call(fun,c(list(data.in[,i]),dots))})
  } else {
    # Select between Croston, SBA and SES
    # Distribute ellipsis (...) inputs to each method
    valid.sexsm <- c("h","w","init","cost","init.opt","outplot","na.rm","probs","qtype")
    valid.crost <- c(valid.sexsm,"nop")
    dots.crost <- dots[names(dots) %in% valid.crost]
    dots.sexsm <- dots[names(dots) %in% valid.sexsm]
    # Perform classification
    cls <- idclass(data.in,type="PKa",outplot="none")
    mdl <- rep("ses",N)
    mdl[cls$idx.croston] <- "croston"
    mdl[cls$idx.sba] <- "sba"
    out <- lapply(1:N,function(i){
      if (mdl[i] == "ses"){
        do.call(sexsm,c(list(data.in[,i]),dots.sexsm))
      } else {
        do.call(crost,c(list(data.in[,i],type=mdl[i]),dots.crost))
      }
    })
  }
  names(out) <- data.names

  # Collect the forecasts of all series in a data frame
  frc.out <- do.call(cbind,lapply(out,function(x){x$frc.out}))
  colnames(frc.out) <- data.names
  frc.out <- data.frame(frc.out,check.names=FALSE)

  res <- list(frc.out=frc.out,out=out)

  # Collect the lead time quantiles
  if (!is.null(dots$probs) && !is.null(out[[1]]$lt)){
    lt.out <- do.call(cbind,lapply(out,function(x){x$lt$quantile}))
    colnames(lt.out) <- data.names
    res$lt.out <- data.frame(lt.out,check.names=FALSE)
  }

  return(res)

}
