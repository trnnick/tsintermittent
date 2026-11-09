#' Croston's method and variants
#'
#' @description Croston's method and its variants, the Syntetos-Boylan approximation
#' (SBA) and the Shale-Boylan-Johnston correction (SBJ), for intermittent demand time
#' series, with fixed or optimised smoothing parameters and initial values.
#'
#' @param data Intermittent demand time series. This can be a vector, a \code{ts}
#'   object, or a data frame, in which case only the first column is used.
#' @param h Forecast horizon. When \code{probs} is provided, \code{h} is understood as
#'   the lead time. See the section "Lead time quantiles".
#' @param w Smoothing parameters. If \code{w == NULL} then the parameters are
#'   optimised. If \code{w} is a single parameter, then the same is used for smoothing
#'   both the demand and the intervals. If two parameters are provided, then the second
#'   is used to smooth the intervals.
#' @param init Initial values for demand and intervals. This can be:
#'   \itemize{
#'   \item \code{c(z,x)}: a vector of two scalars, where the first is the initial
#'   demand and the second is the initial interval;
#'   \item \code{"naive"}: the initial demand is the first non-zero demand and the
#'   initial interval is the first interval;
#'   \item \code{"mean"}: same as \code{"naive"}, but the initial interval is the mean
#'   of all in-sample intervals.
#'   }
#'   If \code{init.opt == TRUE}, these are the starting values of the optimisation.
#' @param nop Number of model parameters, used only if they are optimised. \code{1} uses
#'   the same parameter for the demand and the intervals, while \code{2} uses separate
#'   parameters. There is no reason to expect the demand and the intervals to change at
#'   the same rate, so \code{2} is recommended.
#' @param type Croston's method variant: \code{"croston"} for Croston's method,
#'   \code{"sba"} for the Syntetos-Boylan approximation, and \code{"sbj"} for the
#'   Shale-Boylan-Johnston correction.
#' @param cost Cost function used for optimisation: \code{"mar"} for the mean absolute
#'   rate, \code{"msr"} for the mean squared rate, \code{"mae"} for the mean absolute
#'   error, \code{"mse"} for the mean squared error, \code{"msce"} for the mean squared
#'   cumulative error, and \code{"mace"} for the mean absolute cumulative error. See the
#'   section "Cost functions".
#' @param init.opt If \code{TRUE}, the initial values are optimised.
#' @param outplot Plot of the forecast: \code{FALSE} for no plot, \code{TRUE} or
#'   \code{"period"} for the per-period view, and \code{"leadtime"} for the lead time
#'   view. Both views, with additional options, are available afterwards with
#'   \code{\link{plot.tsint}}.
#' @param opt.on This was meant to be used only by the optimisation of earlier versions
#'   of the package. When \code{TRUE}, no optimisation and no checks on the inputs are
#'   performed. \strong{This argument is deprecated and will be removed in a future
#'   version of the package.}
#' @param na.rm If \code{TRUE}, NA values are removed before modelling.
#' @param probs Probabilities of the quantiles of the demand over the lead time
#'   \code{h}, for example \code{c(0.9,0.95)}. If \code{NULL}, no quantiles are
#'   calculated. See the section "Lead time quantiles".
#' @param qtype Type of empirical quantile calculation, passed to
#'   \code{\link[stats]{quantile}}.
#' @param model A model previously fitted with this function. Its smoothing
#'   parameters, initial values, variant, and cost function are used, without
#'   optimisation, to produce forecasts for \code{data}. See the section "Reusing a
#'   fitted model".
#'
#' @details The forecasts of Croston's method and its variants are demand rates: the
#'   average demand per period, rather than the demand expected in each period. For
#'   example, a forecast of 0.5 implies one unit of demand every two periods. The
#'   forecast is constant over the horizon.
#'
#'   Croston's method updates the non-zero demand and interval estimates only in periods
#'   with demand, so the in-sample forecast \code{frc.in} is \code{NA} up to and
#'   including the period of the first demand.
#'
#' @section Cost functions:
#' Conventional cost functions compare the forecast with the demand of each period.
#' For intermittent demand this favours forecasts close to zero, and compares a demand
#' rate with realised demand, which are not of the same nature. The package offers
#' alternatives that compare like with like.
#' \itemize{
#' \item \code{"mar"} and \code{"msr"} compare the in-sample demand rate with the
#' cumulative mean of the series up to each period (Kourentzes, 2014). The cumulative
#' mean is winsorised over the first 30\% of the sample, where it is based on few
#' observations. These are the recommended cost functions.
#' \item \code{"msce"} and \code{"mace"} use the cumulative forecast errors over
#' \code{h} periods from every in-sample origin, the same errors used for the lead time
#' quantiles. They need \code{h} of at least 1, and \strong{the optimised parameters
#' depend on \code{h}}. Cost functions on multiple steps ahead errors shrink the
#' smoothing parameters towards zero, more strongly for longer horizons, and the
#' shrinkage can become excessive when the sample is short and the horizon is long
#' (Svetunkov, Kourentzes, and Killick, 2024). Use them with lead times that are small
#' relative to the length of the series.
#' \item \code{"mse"} and \code{"mae"} are the conventional cost functions. \code{"mae"}
#' is not recommended for intermittent demand, as it is particularly prone to favouring
#' forecasts close to zero.
#' }
#'
#' @section Lead time quantiles:
#' The forecasts are demand rates, so a predictive distribution for the demand of a
#' single period is not meaningful. What is meaningful, and what matters for stock
#' control, is the cumulative demand over the lead time. When \code{probs} is provided,
#' \code{h} is understood as the lead time. More precisely, \code{h} should be the period
#' the stock decision must cover: the lead time under continuous review, or the lead time
#' plus the review period (the protection interval) under periodic review.
#'
#' The cumulative errors over \code{h} periods are calculated from every in-sample
#' origin, and their empirical quantiles are added to the cumulative forecast over the
#' next \code{h} periods. The forecast errors at different steps ahead are correlated,
#' and the empirical distribution of the cumulative errors includes these covariances
#' without assuming a model for the demand (Saoud, Kourentzes, and Boylan, 2022). Reading
#' the quantiles directly from this distribution also avoids assuming a distribution for
#' the errors.
#'
#' The resulting quantile is the stock needed to cover the demand over the lead time
#' with the requested probability, i.e., the order-up-to level under periodic review or
#' the reorder point under continuous review. The safety stock is the part of it above
#' the cumulative forecast. Since demand cannot be negative, quantiles are truncated at
#' zero. Probabilities below 0.5 are permitted, but are of little use in this setting:
#' they result in negative safety stock and describe the lower tail of the demand over
#' the lead time, which is not relevant to stock control.
#'
#' The empirical quantiles are only as reliable as the number of cumulative errors
#' behind them, and a warning is given when there are fewer than \code{1/(1-max(probs))}.
#'
#' @section Reusing a fitted model:
#' A fitted model can be applied to new data with \code{model}, for example when new
#' observations become available, without optimising the parameters again. The
#' smoothing parameters, initial values, variant, and cost function of the model are
#' used, and any of these inputs given in the call are ignored, except \code{init.opt}.
#' By default the initial values are not optimised, which is appropriate when
#' \code{data} extends the series the model was fitted to, as both start at the same
#' period. To apply the parameters of the model to a different series, use
#' \code{init.opt=TRUE}, so that the initial values are optimised for \code{data},
#' while the smoothing parameters are kept fixed. The same is available with
#' \code{\link{forecast.tsint}}.
#'
#' @return An object of class \code{tsint}, which is a list containing:
#' \item{model}{Type of model fitted.}
#' \item{frc.in}{In-sample demand rate.}
#' \item{frc.out}{Out-of-sample demand rate.}
#' \item{weights}{Smoothing parameters for demand and interval.}
#' \item{initial}{Initialisation values for demand and interval smoothing.}
#' \item{components}{List of \code{c.in} and \code{c.out} containing the non-zero
#'   demand and interval vectors for in- and out-of-sample respectively. The third
#'   element is the coefficient used to scale the demand rate for \code{"sba"} and
#'   \code{"sbj"}.}
#' \item{lt}{Only when \code{probs} is provided. A list with the lead time \code{h},
#'   the probabilities \code{probs}, the cumulative forecast over the lead time
#'   \code{frc}, the quantiles of the demand over the lead time \code{quantile}, the
#'   safety stock \code{ss}, the in-sample cumulative errors \code{errors}, their
#'   origins \code{origin}, their number \code{n}, and the quantile type \code{qtype}.
#'   See \code{\link{lt.dist}} for details.}
#' \item{fit.info}{Information used by the methods of the object, such as the series,
#'   the forecast from the end of the sample, and the cost function. Not intended for
#'   direct use.}
#' The object can be used with \code{print}, \code{summary}, \code{plot}, \code{coef},
#' \code{fitted}, and \code{forecast}.
#'
#' @references
#' Optimisation of the methods and the MAR and MSR cost functions:
#' Kourentzes, N. (2014). On intermittent demand model optimisation and selection.
#' \emph{International Journal of Production Economics}, 156, 180--190.
#' \doi{10.1016/j.ijpe.2014.06.007}
#'
#' Shrinkage of multiple steps ahead cost functions:
#' Svetunkov, I., Kourentzes, N., and Killick, R. (2024). Multi-step estimators and
#' shrinkage effect in time series models. \emph{Computational Statistics}, 39(3),
#' 1203--1239. \doi{10.1007/s00180-023-01377-x}
#'
#' Cumulative errors for the lead time distribution:
#' Saoud, P., Kourentzes, N., and Boylan, J. E. (2022). Approximations for the lead time
#' variance: A forecasting and inventory evaluation. \emph{Omega}, 110, 102614.
#' \doi{10.1016/j.omega.2022.102614}
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{tsb}}, \code{\link{sexsm}}, \code{\link{crost.ma}},
#'   \code{\link{imapa}}, \code{\link{plot.tsint}}, \code{\link{lt.dist}}.
#'
#' @examples
#' # Croston's method with optimised parameters and initial values
#' fit <- crost(ts.data1)
#' fit
#' summary(fit)
#'
#' # SBA with a plot of the forecast
#' crost(ts.data1, type="sba", outplot=TRUE)
#'
#' # Same parameter for demand and intervals
#' crost(ts.data1, type="sba", nop=1)$weights
#'
#' # Different cost functions
#' crost(ts.data1, type="sba", cost="msr")$weights
#' crost(ts.data1, type="sba", cost="mse")$weights
#'
#' # Fixed smoothing parameters, with optimised initial values
#' crost(ts.data1, w=c(0.1,0.05))$initial
#'
#' # Fixed smoothing parameters and initial values
#' crost(ts.data1, w=c(0.1,0.05), init=c(5,3), init.opt=FALSE)$frc.out
#'
#' # Initial values as the first demand and interval, not optimised
#' crost(ts.data1, w=0.1, init="naive", init.opt=FALSE)$initial
#'
#' # Quantiles of the demand over a lead time of 3 periods
#' fit <- crost(ts.data1, h=3, type="sba", probs=c(0.9,0.95))
#' fit$lt$quantile  # Order-up-to level or reorder point
#' fit$lt$ss        # Safety stock
#' plot(fit, view="leadtime")
#'
#' # Optimise on the cumulative errors over the lead time
#' crost(ts.data1, h=3, type="sba", cost="msce", probs=0.95)$weights
#'
#' # Reuse a fitted model on an updated series, without optimisation
#' fit <- crost(window(ts.data1, end=c(1985,12)), type="sba")
#' crost(ts.data1, model=fit)$frc.out
#'
#' # Use the smoothing parameters of the model on another series
#' crost(ts.data2, model=fit, init.opt=TRUE)$initial
#'
#' @export crost
crost <- function(data,h=10,w=NULL,init=c("mean","naive"),nop=c(2,1),
                  type=c("croston","sba","sbj"),cost=c("mar","msr","mae","mse","msce","mace"),
                  init.opt=c(TRUE,FALSE),outplot=c(FALSE,TRUE),opt.on=c(FALSE,TRUE),
                  na.rm=c(FALSE,TRUE),probs=NULL,qtype=7,model=NULL){

  # Reuse a fitted model
  if (!is.null(model)){
    model <- id.model(model,"crost")
    w <- model$weights
    init <- model$initial
    type <- model$model
    cost <- model$fit.info$cost
    if (missing(init.opt)){
      init.opt <- FALSE
    }
  }

  # Defaults
  h <- id.h(h)
  type <- id.match(type,c("croston","sba","sbj"),"type",alias=c(crost="croston"))
  cost <- id.match(cost,c("mar","msr","mae","mse","msce","mace"),"cost")
  init.opt <- init.opt[1]
  outplot <- id.outplot(outplot)
  opt.on <- opt.on[1]
  na.rm <- na.rm[1]
  nop <- nop[1]
  probs <- id.probs(probs,h)
  if (!is.numeric(init)){
    init <- id.match(init,c("mean","naive"),"init")
  } else {
    if (length(init)<2){
      stop("A numeric init must contain two values: the initial demand and the initial interval.")
    }
    init <- init[1:2]
  }

  # Make sure that nop is of correct length
  if (nop>2 || nop<1){
    nop <- 2
    warning("nop can be either 1 or 2. Overriden to 2.")
  }

  # Prepare data
  prep <- id.prep(data,na.rm)
  data <- prep$y
  n <- length(data)

  # Check number of non-zero values - need to have at least two
  if (sum(data!=0,na.rm=TRUE)<2){
    stop("Need at least two non-zero values to model time series.")
  }

  # Croston decomposition
  dec <- crost.decomp(data,init="naive")
  z <- dec$demand
  x <- dec$interval

  # Initialise
  if (!is.numeric(init)){
    if (init=="mean"){
      init <- c(z[1],mean(x))
    } else {
      init <- c(z[1],x[1])
    }
  }

  # Optimise parameters if requested
  w.opt <- is.null(w)
  if (opt.on == FALSE){
    if (is.null(w) || init.opt == TRUE){
      if (cost %in% c("msce","mace") && h<1){
        stop("Cost functions msce and mace need h of at least 1.")
      }
      wopt <- crost.opt(data,type,cost,w,nop,init,init.opt,h)
      w <- wopt$w
      init <- wopt$init
    }
  }

  # Check initial values
  if (opt.on == FALSE){
    if (init[1]<0){
      stop("Initial demand cannot be a negative number.")
    }
    if (init[2]<1){
      stop("Initial interval cannot be less than 1.")
    }
  }

  # Fit model
  fit <- crost.fit(data,w,init,type)

  # Forecast out-of-sample demand rate
  k <- length(fit$cc)
  if (h>0){
    frc.out <- rep(fit$cc[k],h)
    x.out <- rep(fit$xfit[k],h)
    z.out <- rep(fit$zfit[k],h)
  } else {
    frc.out <- x.out <- z.out <- NULL
  }

  # Prepare output - Assign weight to intervals if same with demand
  if (length(w)==1){
    w <- c(w,w)
  }

  # Prepare demand and interval vectors for output
  c.in <- array(cbind(fit$z.in,fit$x.in),c(n,2),dimnames=list(NULL,c("Demand","Interval")))
  if (h>0){
    c.out <- array(cbind(z.out,x.out),c(h,2),dimnames=list(NULL,c("Demand","Interval")))
  } else {
    c.out <- NULL
  }
  comp <- list(c.in=c.in,c.out=c.out,coeff=fit$coeff)

  out <- list(model=type,frc.in=fit$frc.in,frc.out=frc.out,
              weights=w,initial=c(fit$zfit[1],fit$xfit[1]),components=comp)

  # Finalise output: lead time quantiles, class, and plot
  out <- id.finalise(out,"crost",fit$cc[k],data,prep$tsp,h,cost,probs,qtype,
                     opt=c(w=w.opt && !opt.on,init=init.opt && !opt.on),outplot=outplot)

  return(out)

}

#-------------------------------------------------
crost.fit <- function(data,w,init,type){
# Fit Croston's method and variants with given parameters - internal function
#
# Inputs:
#   data        Intermittent demand time series (numeric vector).
#   w           Smoothing parameters, one or two.
#   init        Initial demand and interval.
#   type        "croston", "sba", or "sbj".
#
# Outputs:
#   frc.in      In-sample demand rate.
#   cc          Demand rate after each non-zero demand. The last one is the forecast.
#   zfit, xfit  Smoothed non-zero demand and intervals.
#   z.in, x.in  Smoothed non-zero demand and intervals at each period.
#   coeff       Coefficient scaling the demand rate for SBA and SBJ.

  n <- length(data)

  # Croston decomposition
  nzd <- which(data != 0)               # Find location on non-zero demand
  k <- length(nzd)
  z <- data[nzd]                        # Demand
  x <- c(nzd[1],nzd[2:k]-nzd[1:(k-1)])  # Intervals

  # Pre-allocate memory
  zfit <- vector("numeric",k)
  xfit <- vector("numeric",k)
  zfit[1] <- init[1]
  xfit[1] <- init[2]

  if (length(w)==1){
    a.demand <- w[1]
    a.interval <- w[1]
  } else {
    a.demand <- w[1]
    a.interval <- w[2]
  }

  # Set coefficient
  if (type == "sba"){
    coeff <- 1-(a.interval/2)
  } else if (type == "sbj"){
    coeff <- (1-a.interval/(2-a.interval))
  } else {
    coeff <- 1
  }

  # Fit model
  for (i in 2:k){
    zfit[i] <- zfit[i-1] + a.demand * (z[i] - zfit[i-1])      # Demand
    xfit[i] <- xfit[i-1] + a.interval * (x[i] - xfit[i-1])    # Interval
  }
  cc <- coeff * zfit/xfit

  # Calculate in-sample demand rate
  frc.in <- x.in <- z.in <- rep(NA,n)
  tv <- c(nzd+1,n)  # Time vector used to create frc.in forecasts
  for (i in 1:k){
    if (tv[i]<=n){
      frc.in[tv[i]:min(c(tv[i+1],n))] <- cc[i]
      x.in[tv[i]:min(c(tv[i+1],n))] <- xfit[i]
      z.in[tv[i]:min(c(tv[i+1],n))] <- zfit[i]
    }
  }

  return(list(frc.in=frc.in,cc=cc,zfit=zfit,xfit=xfit,z.in=z.in,x.in=x.in,coeff=coeff))

}

#-------------------------------------------------
crost.opt <- function(data,type,cost,w=NULL,nop=2,init,init.opt=TRUE,h=0){
# Optimisation function for Croston and variants - internal function
#
# Outputs:
#   w           Smoothing parameters.
#   init        Initial values.

  # Croston decomposition
  nzd <- which(data != 0)
  k <- length(nzd)
  x <- c(nzd[1],nzd[2:k]-nzd[1:(k-1)])  # Intervals

  w.opt <- is.null(w)
  if (w.opt == FALSE){
    nop <- length(w)
  }

  # Cost function of the vector of free parameters
  fn <- function(p0,lbound,ubound){
    if (w.opt == TRUE && init.opt == TRUE){
      wp <- p0[1:nop]
      ip <- p0[(nop+1):(nop+2)]
    } else if (w.opt == TRUE){
      wp <- p0
      ip <- init
    } else {
      wp <- w
      ip <- p0
    }
    E <- id.cost(data,crost.fit(data,wp,ip,type)$frc.in,cost,h)
    if (id.outbounds(p0,lbound,ubound)){
      E <- 9*10^99
    }
    return(E)
  }

  if (w.opt == TRUE && init.opt == FALSE){
    # Optimise only w
    lbound <- rep(0,nop)
    ubound <- rep(1,nop)
    wopt <- id.optim(function(p){fn(p,lbound,ubound)},rep(0.05,nop),lbound,ubound,
                     brent=(nop==1))
    wopt <- c(wopt,init)
  } else if (w.opt == TRUE && init.opt == TRUE){
    # Optimise w and init
    lbound <- c(rep(0,nop),0,1)
    ubound <- c(rep(1,nop),max(data),max(x))
    wopt <- id.optim(function(p){fn(p,lbound,ubound)},c(rep(0.05,nop),init[1],init[2]),
                     lbound,ubound)
  } else {
    # Optimise only init
    lbound <- c(0,1)
    ubound <- c(max(data),max(x))
    wopt <- id.optim(function(p){fn(p,lbound,ubound)},c(init[1],init[2]),lbound,ubound)
    wopt <- c(w,wopt)
  }

  return(list(w=wopt[1:nop],init=wopt[(nop+1):(nop+2)]))

}
