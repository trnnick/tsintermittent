#' Simple exponential smoothing
#'
#' @description Simple exponential smoothing (SES) with fixed or optimised smoothing
#' parameter and initial value, using the cost functions of the package.
#'
#' @inheritParams crost
#' @param w Smoothing parameter. If \code{w == NULL} then the parameter is optimised.
#' @param init Initial value. This can be:
#'   \itemize{
#'   \item a numeric value for the initial level;
#'   \item \code{"naive"}: the initial level is the first observation;
#'   \item \code{"mean"}: the initial level is the mean of the series.
#'   }
#'   If \code{init.opt == TRUE}, this is the starting value of the optimisation.
#'
#' @details SES makes no special treatment of periods with zero demand. It is a common
#'   benchmark for intermittent demand, and it is used by \code{\link{imapa}} at
#'   aggregation levels without zero demand. Its forecast is constant over the horizon.
#'   The cost functions MAR and MSR were designed for methods that forecast a demand
#'   rate, such as Croston's method and its variants, so their benefits may not carry
#'   over to SES.
#'
#'   Unlike \code{\link{crost}} and \code{\link{tsb}}, the initial value of SES is the
#'   forecast for the first period, so \code{frc.in} has no missing values.
#'
#' @inheritSection crost Cost functions
#' @inheritSection crost Lead time quantiles
#' @inheritSection crost Reusing a fitted model
#'
#' @return An object of class \code{tsint}, which is a list containing:
#' \item{model}{Type of model fitted.}
#' \item{frc.in}{In-sample demand.}
#' \item{frc.out}{Out-of-sample demand.}
#' \item{alpha}{Smoothing parameter.}
#' \item{initial}{Initialisation value.}
#' \item{lt}{Only when \code{probs} is provided. See \code{\link{crost}}.}
#' \item{fit.info}{Information used by the methods of the object, such as the series,
#'   the forecast from the end of the sample, and the cost function. Not intended for
#'   direct use.}
#' The object can be used with \code{print}, \code{summary}, \code{plot}, \code{coef},
#' \code{fitted}, and \code{forecast}.
#'
#' @references
#' The references for the cost functions and the lead time quantiles are given in
#' \code{\link{crost}}.
#'
#' Kourentzes, N. (2014). On intermittent demand model optimisation and selection.
#' \emph{International Journal of Production Economics}, 156, 180--190.
#' \doi{10.1016/j.ijpe.2014.06.007}
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{crost}}, \code{\link{tsb}}, \code{\link{plot.tsint}}.
#'
#' @examples
#' # SES with optimised parameter and initial value
#' fit <- sexsm(ts.data1, outplot=TRUE)
#' fit$alpha
#'
#' # Conventional cost function
#' sexsm(ts.data1, cost="mse")$alpha
#'
#' # Fixed parameter and initial value
#' sexsm(ts.data1, w=0.1, init="mean", init.opt=FALSE)$frc.out
#'
#' # Reuse the fitted model on an updated series
#' fit <- sexsm(window(ts.data1, end=c(1985,12)))
#' sexsm(ts.data1, model=fit)$frc.out
#'
#' # Quantiles of the demand over a lead time of 3 periods
#' sexsm(ts.data1, h=3, probs=0.95)$lt
#'
#' @export sexsm
sexsm <- function(data,h=10,w=NULL,init=c("mean","naive"),
                  cost=c("mar","msr","mae","mse","msce","mace"),
                  init.opt=c(TRUE,FALSE),outplot=c(FALSE,TRUE),opt.on=c(FALSE,TRUE),
                  na.rm=c(FALSE,TRUE),probs=NULL,qtype=7,model=NULL){

  # Reuse a fitted model
  if (!is.null(model)){
    model <- id.model(model,"sexsm")
    w <- model$alpha
    init <- model$initial
    cost <- model$fit.info$cost
    if (missing(init.opt)){
      init.opt <- FALSE
    }
  }

  # Defaults
  h <- id.h(h)
  w <- w[1]
  cost <- id.match(cost,c("mar","msr","mae","mse","msce","mace"),"cost")
  init.opt <- init.opt[1]
  outplot <- id.outplot(outplot)
  opt.on <- opt.on[1]
  na.rm <- na.rm[1]
  probs <- id.probs(probs,h)
  if (!is.numeric(init)){
    init <- id.match(init,c("mean","naive"),"init")
  } else {
    init <- init[1]
  }

  # Prepare data
  prep <- id.prep(data,na.rm)
  data <- prep$y
  n <- length(data)

  # Initialise
  if (!is.numeric(init)){
    if (init=="mean"){
      init <- mean(data)
    } else {
      init <- data[1]
    }
  }

  # Optimise parameters if requested
  w.opt <- is.null(w)
  if (opt.on == FALSE){
    if (is.null(w) || init.opt == TRUE){
      if (cost %in% c("msce","mace") && h<1){
        stop("Cost functions msce and mace need h of at least 1.")
      }
      wopt <- sexsm.opt(data,cost,w,init,init.opt,h)
      w <- wopt$w
      init <- wopt$init
    }
  }

  # Fit model
  fit <- sexsm.fit(data,w,init)

  # Forecast out-of-sample demand rate
  if (h>0){
    frc.out <- rep(fit$fit[n+1],h)
  } else {
    frc.out <- NULL
  }

  out <- list(model="sexsm",frc.in=fit$frc.in,frc.out=frc.out,
              alpha=w,initial=init)

  # Finalise output: lead time quantiles, class, and plot
  out <- id.finalise(out,"sexsm",fit$fit[n+1],data,prep$tsp,h,cost,probs,qtype,
                     opt=c(w=w.opt && !opt.on,init=init.opt && !opt.on),outplot=outplot)

  return(out)

}

#-------------------------------------------------
sexsm.fit <- function(data,w,init){
# Fit SES with given parameter - internal function
#
# Outputs:
#   frc.in      In-sample forecast.
#   fit         Level after each period, starting from the initial value. The last
#               one is the forecast.

  n <- length(data)

  # Pre-allocate memory
  fit <- vector("numeric",n+1)
  fit[1] <- init[1]

  # Fit model
  for (i in 2:(n+1)){
    fit[i] <- w*data[i-1] + (1-w)*fit[i-1]
  }

  return(list(frc.in=fit[1:n],fit=fit))

}

#-------------------------------------------------
sexsm.opt <- function(data,cost,w=NULL,init,init.opt=TRUE,h=0){
# Optimisation function for SES - internal function

  w.opt <- is.null(w)

  # Cost function of the vector of free parameters
  fn <- function(p0,lbound,ubound){
    if (w.opt == TRUE && init.opt == TRUE){
      wp <- p0[1]
      ip <- p0[2]
    } else if (w.opt == TRUE){
      wp <- p0[1]
      ip <- init
    } else {
      wp <- w
      ip <- p0[1]
    }
    E <- id.cost(data,sexsm.fit(data,wp,ip)$frc.in,cost,h)
    if (init.opt == TRUE){
      if (id.outbounds(p0,lbound,ubound)){
        E <- 9*10^99
      }
    }
    return(E)
  }

  if (w.opt == TRUE && init.opt == TRUE){
    # Optimise w and init
    lbound <- c(0,0)
    ubound <- c(1,max(data))
    wopt <- id.optim(function(p){fn(p,lbound,ubound)},c(0.5,init[1]),lbound,ubound)
  } else if (w.opt == TRUE && init.opt == FALSE){
    # Optimise only w
    lbound <- 0
    ubound <- 1
    wopt <- id.optim(function(p){fn(p,lbound,ubound)},0.5,lbound,ubound,brent=TRUE)
    wopt <- c(wopt,init)
  } else {
    # Optimise only init
    lbound <- 0
    ubound <- max(data)
    wopt <- id.optim(function(p){fn(p,lbound,ubound)},init[1],lbound,ubound,brent=TRUE)
    wopt <- c(w,wopt)
  }

  return(list(w=wopt[1],init=wopt[2]))

}
