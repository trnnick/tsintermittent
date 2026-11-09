#' TSB method
#'
#' @description The Teunter-Syntetos-Babai (TSB) method for intermittent demand time
#' series, with fixed or optimised smoothing parameters and initial values.
#'
#' @inheritParams crost
#' @param w Smoothing parameters. If \code{w == NULL} then the parameters are
#'   optimised. Otherwise, a vector of two values: the first is the smoothing parameter
#'   for the non-zero demand and the second for the demand probability.
#' @param init Initial values for demand and demand probability. This can be:
#'   \itemize{
#'   \item \code{c(z,p)}: a vector of two scalars, where the first is the initial
#'   demand and the second is the initial demand probability;
#'   \item \code{"naive"}: the initial demand is the first non-zero demand and the
#'   initial demand probability is that of the first period;
#'   \item \code{"mean"}: same as \code{"naive"}, but the initial demand probability is
#'   the mean of all in-sample probabilities.
#'   }
#'   If \code{init.opt == TRUE}, these are the starting values of the optimisation.
#'
#' @details TSB replaces the intervals of Croston's method with the probability of
#'   demand, which is updated in every period, while the non-zero demand is updated
#'   only when demand occurs. The forecast is the product of the two, a demand rate,
#'   and is constant over the horizon. As the probability is updated in every period,
#'   TSB reacts to items that stop being demanded. When the parameters are optimised,
#'   the smoothing parameter of the demand probability is kept no larger than that of
#'   the non-zero demand, following Teunter, Syntetos, and Babai (2011).
#'
#' @inheritSection crost Cost functions
#' @inheritSection crost Lead time quantiles
#' @inheritSection crost Reusing a fitted model
#'
#' @return An object of class \code{tsint}, which is a list containing:
#' \item{model}{Type of model fitted.}
#' \item{frc.in}{In-sample demand rate.}
#' \item{frc.out}{Out-of-sample demand rate.}
#' \item{weights}{Smoothing parameters for demand and demand probability.}
#' \item{initial}{Initialisation values for demand and demand probability smoothing.}
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
#' Teunter, R. H., Syntetos, A. A., and Babai, M. Z. (2011). Intermittent demand:
#' linking forecasting to inventory obsolescence. \emph{European Journal of Operational
#' Research}, 214(3), 606--615.
#'
#' Kourentzes, N. (2014). On intermittent demand model optimisation and selection.
#' \emph{International Journal of Production Economics}, 156, 180--190.
#' \doi{10.1016/j.ijpe.2014.06.007}
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{crost}}, \code{\link{sexsm}}, \code{\link{plot.tsint}}.
#'
#' @examples
#' # TSB with optimised parameters and initial values
#' fit <- tsb(ts.data1, outplot=TRUE)
#' fit$weights
#'
#' # Different cost function
#' tsb(ts.data1, cost="msr")$weights
#'
#' # Fixed parameters for the demand and the demand probability
#' tsb(ts.data1, w=c(0.2,0.05))$initial
#'
#' # Fixed parameters and initial values
#' tsb(ts.data1, w=c(0.2,0.05), init=c(4,0.3), init.opt=FALSE)$frc.out
#'
#' # Reuse the fitted model on an updated series
#' fit <- tsb(window(ts.data1, end=c(1985,12)))
#' tsb(ts.data1, model=fit)$frc.out
#'
#' # Quantiles of the demand over a lead time of 4 periods
#' fit <- tsb(ts.data1, h=4, probs=c(0.9,0.95), outplot="leadtime")
#' fit$lt$quantile
#'
#' @export tsb
tsb <- function(data,h=10,w=NULL,init=c("mean","naive"),
                cost=c("mar","msr","mae","mse","msce","mace"),
                init.opt=c(TRUE,FALSE),outplot=c(FALSE,TRUE),
                opt.on=c(FALSE,TRUE),na.rm=c(FALSE,TRUE),probs=NULL,qtype=7,model=NULL){

  # Reuse a fitted model
  if (!is.null(model)){
    model <- id.model(model,"tsb")
    w <- model$weights
    init <- model$initial
    cost <- model$fit.info$cost
    if (missing(init.opt)){
      init.opt <- FALSE
    }
  }

  # Defaults
  h <- id.h(h)
  cost <- id.match(cost,c("mar","msr","mae","mse","msce","mace"),"cost")
  init.opt <- init.opt[1]
  outplot <- id.outplot(outplot)
  opt.on <- opt.on[1]
  na.rm <- na.rm[1]
  probs <- id.probs(probs,h)
  if (!is.numeric(init)){
    init <- id.match(init,c("mean","naive"),"init")
  } else {
    if (length(init)<2){
      stop("A numeric init must contain two values: the initial demand and the initial demand probability.")
    }
    init <- init[1:2]
  }
  if (!is.null(w) && length(w)!=2){
    stop(paste("w must be a vector of 2 elements: the smoothing parameter",
               " for the non-zero demand and the parameter for the ",
               "probability of demand.",sep=""))
  }

  # Prepare data
  prep <- id.prep(data,na.rm)
  data <- prep$y
  n <- length(data)

  # TSB decomposition
  p <- as.numeric(data!=0)    # Demand probability
  z <- data[data!=0]          # Non-zero demand

  if (length(z)<1){
    stop("Need at least one non-zero value to model time series.")
  }

  # Initialise
  if (!is.numeric(init)){
    if (init=="mean"){
      init <- c(z[1],mean(p))
    } else {
      init <- c(z[1],p[1])
    }
  }

  # Optimise parameters if requested
  w.opt <- is.null(w)
  if (opt.on == FALSE){
    if (is.null(w) || init.opt == TRUE){
      if (cost %in% c("msce","mace") && h<1){
        stop("Cost functions msce and mace need h of at least 1.")
      }
      wopt <- tsb.opt(data,cost,w,init,init.opt,h)
      w <- wopt$w
      init <- wopt$init
    }
  }

  # Check initial values
  if (opt.on == FALSE){
    if (init[1]<0){
      stop("Initial demand cannot be a negative number.")
    }
    if (init[2]<0){
      stop("Initial demand probability cannot be a negative number.")
    }
  }

  # Fit model
  fit <- tsb.fit(data,w,init)

  if (h>0){
    frc.out <- rep(fit$yfit[n],h)
  } else {
    frc.out <- NULL
  }

  out <- list(model="tsb",frc.in=fit$frc.in,frc.out=frc.out,
              weights=w,initial=c(fit$zfit[1],fit$pfit[1]))

  # Finalise output: lead time quantiles, class, and plot
  out <- id.finalise(out,"tsb",fit$yfit[n],data,prep$tsp,h,cost,probs,qtype,
                     opt=c(w=w.opt && !opt.on,init=init.opt && !opt.on),outplot=outplot)

  return(out)

}

#-------------------------------------------------
tsb.fit <- function(data,w,init){
# Fit TSB with given parameters - internal function
#
# Inputs:
#   data        Intermittent demand time series (numeric vector).
#   w           Smoothing parameters for non-zero demand and demand probability.
#   init        Initial demand and demand probability.
#
# Outputs:
#   frc.in      In-sample demand rate.
#   yfit        Demand rate after each period. The last one is the forecast.
#   zfit, pfit  Smoothed non-zero demand and demand probability.

  n <- length(data)
  p <- as.numeric(data!=0)

  # Pre-allocate memory
  zfit <- vector("numeric",n)
  pfit <- vector("numeric",n)
  zfit[1] <- init[1]
  pfit[1] <- init[2]

  # Fit model
  for (i in 2:n){
    pfit[i] <- pfit[i-1] + w[2]*(p[i]-pfit[i-1])        # Demand probability
    if (p[i]==0){
      zfit[i] <- zfit[i-1]
    } else {
      zfit[i] <- zfit[i-1] + w[1]*(data[i]-zfit[i-1])   # Demand
    }
  }
  yfit <- pfit*zfit
  frc.in <- c(NA,yfit[1:(n-1)])

  return(list(frc.in=frc.in,yfit=yfit,zfit=zfit,pfit=pfit))

}

#-------------------------------------------------
tsb.opt <- function(data,cost,w=NULL,init,init.opt=TRUE,h=0){
# Optimisation function for TSB - internal function

  w.opt <- is.null(w)

  # Cost function of the vector of free parameters
  fn <- function(p0,lbound,ubound){
    if (w.opt == TRUE && init.opt == TRUE){
      wp <- p0[1:2]
      ip <- p0[3:4]
    } else if (w.opt == TRUE){
      wp <- p0[1:2]
      ip <- init
    } else {
      wp <- w
      ip <- p0[1:2]
    }
    E <- id.cost(data,tsb.fit(data,wp,ip)$frc.in,cost,h)
    if (id.outbounds(p0,lbound,ubound)){
      E <- 9*10^99
    }
    # Parameter of demand probability must be smaller than parameter of demand
    if (w.opt == TRUE){
      if (p0[1] < p0[2]){
        E <- 9*10^99
      }
    }
    return(E)
  }

  if (w.opt == TRUE && init.opt == TRUE){
    # Optimise w and init
    lbound <- c(0,0,0,0)
    ubound <- c(1,1,max(data),1)
    wopt <- id.optim(function(p){fn(p,lbound,ubound)},c(rep(0.05,2),init[1],init[2]),
                     lbound,ubound)
  } else if (w.opt == TRUE && init.opt == FALSE){
    # Optimise only w
    lbound <- c(0,0)
    ubound <- c(1,1)
    wopt <- id.optim(function(p){fn(p,lbound,ubound)},rep(0.05,2),lbound,ubound)
    wopt <- c(wopt,init)
  } else {
    # Optimise only init
    lbound <- c(0,0)
    ubound <- c(max(data),1)
    wopt <- id.optim(function(p){fn(p,lbound,ubound)},c(init[1],init[2]),lbound,ubound)
    wopt <- c(w,wopt)
  }

  return(list(w=wopt[1:2],init=wopt[3:4]))

}
