#' @importFrom generics forecast
#' @export
generics::forecast

#-------------------------------------------------
#' Forecast from a fitted intermittent demand model
#'
#' @description Point forecasts from a model fitted with \code{\link{crost}},
#' \code{\link{tsb}}, \code{\link{sexsm}}, \code{\link{crost.ma}}, or
#' \code{\link{imapa}}, for any horizon, without optimising the model again. The fitted
#' model can also be applied to new data.
#'
#' @param object A fitted model, of class \code{tsint}.
#' @param h Forecast horizon. If \code{NULL}, the horizon of the fitted model is used.
#' @param newdata Optional time series to apply the fitted model to, for example the
#'   series updated with new observations. The parameters and initial values of the
#'   model are used without optimisation, see the section "Reusing a fitted model" in
#'   \code{\link{crost}}. If \code{NULL}, the forecasts are produced from the end of the
#'   series the model was fitted to.
#' @param ... Unused.
#'
#' @details This method gives only the point forecasts, i.e., the expected demand per
#'   period. For Croston's method and its variants, TSB, and iMAPA these are demand
#'   rates, constant over the horizon. No prediction intervals are provided. For these
#'   methods a predictive distribution is meaningful only for the demand over the lead
#'   time, not for the demand of each period, so per-period intervals would be
#'   misleading. Note that the parameters of a model optimised with the cost functions
#'   \code{"msce"} or \code{"mace"} depend on the horizon used when fitting it, and
#'   they are not reoptimised for a new \code{h}.
#'
#'   Everything else is available in the native functions of the package:
#'   \itemize{
#'   \item quantiles of the demand over the lead time and the safety stock, with the
#'   input \code{probs} of the forecasting functions, e.g.,
#'   \code{crost(y, h=3, probs=c(0.9,0.95))$lt}, or for an existing fitted model, for
#'   any lead time and probabilities, without refitting, with
#'   \code{\link{lt.dist}}, e.g., \code{lt.dist(fit, h=3, probs=0.95)};
#'   \item plots of the forecasts per period and over the lead time, with
#'   \code{\link{plot.tsint}} and \code{\link{plot.imapa}};
#'   \item the parameters and a summary of the model, with \code{coef} and
#'   \code{summary}, see \code{\link{tsint-methods}};
#'   \item the fitted model applied to new data, keeping its smoothing parameters but
#'   optimising its initial values, with the input \code{model} of the forecasting
#'   functions and \code{init.opt=TRUE}, e.g., \code{crost(y, model=fit, init.opt=TRUE)}.
#'   }
#'
#' @return A vector of point forecasts of length \code{h}. If the series was a \code{ts}
#'   object, the forecasts are a \code{ts} object continuing its time index.
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{lt.dist}}, \code{\link{crost}}.
#'
#' @examples
#' fit <- crost(window(ts.data1, end=c(1985,12)), type="sba")
#'
#' # Point forecasts for a different horizon
#' forecast(fit, h=6)
#'
#' # The fitted model applied to the updated series
#' forecast(fit, h=6, newdata=ts.data1)
#'
#' # Quantiles of the demand over a lead time of 3 periods, in the native way
#' lt.dist(fit, h=3, probs=c(0.9,0.95))
#'
#' @export
forecast.tsint <- function(object,h=NULL,newdata=NULL,...){

  info <- object$fit.info
  if (is.null(h)){
    h <- info$h
  }
  h <- id.h(h)
  if (h<1){
    stop("Input h must be at least 1.")
  }

  if (!is.null(newdata)){
    # Apply the fitted model to the new data, without optimisation
    if (info$method == "imapa"){
      fit <- imapa(newdata,h=h,model.fit=object)
    } else {
      fun <- switch(info$method,"crost"=crost,"tsb"=tsb,"sexsm"=sexsm,"crost.ma"=crost.ma)
      fit <- fun(newdata,h=h,model=object)
    }
    info <- fit$fit.info
  }

  # All methods produce a constant forecast over the horizon
  frc <- rep(info$level,h)
  if (!is.null(info$tsp)){
    frc <- ts(frc,start=info$tsp[2]+1/info$tsp[3],frequency=info$tsp[3])
  }

  return(frc)

}
