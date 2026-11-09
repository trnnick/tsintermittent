#' Croston-style moving averages and variants
#'
#' @description Croston's method and its variants, using moving averages instead of
#' exponential smoothing for the non-zero demand and the intervals, with fixed or
#' optimised orders.
#'
#' @inheritParams crost
#' @param w Moving average orders. If \code{w == NULL} then the orders are optimised.
#'   If \code{w} is a single value, then the same order is used for both the demand and
#'   the intervals. If two values are provided, then the second is used for the
#'   intervals.
#' @param model A model previously fitted with this function. Its orders, variant, and
#'   cost function are used, without optimisation, to produce forecasts for
#'   \code{data}.
#' @param nop Number of model parameters, used only if they are optimised. \code{1} uses
#'   the same order for the demand and the intervals, while \code{2} uses separate
#'   orders.
#'
#' @details The orders are optimised by a grid search over all orders up to the number
#'   of non-zero demands minus one, as the cost functions are not smooth in integer
#'   orders. When several orders give the same minimum cost, the one with the smallest interval
#'   order, and then the smallest demand order, is used. The
#'   forecasts are demand rates, constant over the horizon.
#'
#' @inheritSection crost Cost functions
#' @inheritSection crost Lead time quantiles
#'
#' @return An object of class \code{tsint}, which is a list containing:
#' \item{model}{Type of model fitted.}
#' \item{frc.in}{In-sample demand rate.}
#' \item{frc.out}{Out-of-sample demand rate.}
#' \item{order}{Moving average orders for demand and interval.}
#' \item{components}{List of \code{c.in} and \code{c.out} containing the non-zero
#'   demand and interval vectors for in- and out-of-sample respectively. The third
#'   element is the coefficient used to scale the demand rate for \code{"sba"} and
#'   \code{"sbj"}.}
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
#' @seealso \code{\link{crost}}, \code{\link{plot.tsint}}.
#'
#' @examples
#' # Optimised orders
#' fit <- crost.ma(ts.data1, outplot=TRUE)
#' fit$order
#'
#' # Same order for demand and intervals, SBA variant
#' crost.ma(ts.data1, nop=1, type="sba")$order
#'
#' # Fixed orders
#' crost.ma(ts.data1, w=c(3,2))$frc.out
#'
#' # Reuse the fitted orders on an updated series
#' fit <- crost.ma(window(ts.data1, end=c(1985,12)))
#' crost.ma(ts.data1, model=fit)$order
#'
#' # Quantiles of the demand over a lead time of 2 periods
#' crost.ma(ts.data1, h=2, probs=c(0.9,0.95))$lt$quantile
#'
#' @export crost.ma
crost.ma <- function(data,h=10,w=NULL,nop=c(2,1),type=c("croston","sba","sbj"),
                     cost=c("mar","msr","mae","mse","msce","mace"),outplot=c(FALSE,TRUE),
                     na.rm=c(FALSE,TRUE),probs=NULL,qtype=7,model=NULL){

  # Reuse a fitted model
  if (!is.null(model)){
    model <- id.model(model,"crost.ma")
    w <- model$order
    type <- sub("^ma\\.","",model$model)
    cost <- model$fit.info$cost
  }

  # Defaults
  h <- id.h(h)
  type <- id.match(type,c("croston","sba","sbj"),"type",alias=c(crost="croston"))
  cost <- id.match(cost,c("mar","msr","mae","mse","msce","mace"),"cost")
  nop <- nop[1]
  outplot <- id.outplot(outplot)
  na.rm <- na.rm[1]
  probs <- id.probs(probs,h)

  # Prepare data
  prep <- id.prep(data,na.rm)
  data <- prep$y
  n <- length(data)

  if (sum(data!=0,na.rm=TRUE)<2){
    stop("Need at least two non-zero values to model time series.")
  }

  # Optimise parameters if requested
  w.opt <- is.null(w)
  if (w.opt == TRUE){
    if (cost %in% c("msce","mace") && h<1){
      stop("Cost functions msce and mace need h of at least 1.")
    }
    w <- crost.ma.opt(data,type,cost,nop,length(which(data!=0))-1,h)
  }

  # Assign parameters
  if (length(w)==1){
    k.demand <- w[1]
    k.interval <- w[1]
  } else {
    k.demand <- w[1]
    k.interval <- w[2]
  }

  fit <- crost.ma.fit(data,k.demand,k.interval,type)
  k <- length(fit$cc)

  # Forecast out-of-sample demand rate
  if (h>0){
    frc.out <- rep(fit$cc[k],h)
    x.out <- rep(fit$xfit[k],h)
    z.out <- rep(fit$zfit[k],h)
  } else {
    frc.out <- x.out <- z.out <- NULL
  }

  # Prepare demand and interval vectors for output
  c.in <- array(cbind(fit$z.in,fit$x.in),c(n,2),dimnames=list(NULL,c("Demand","Interval")))
  if (h>0){
    c.out <- array(cbind(z.out,x.out),c(h,2),dimnames=list(NULL,c("Demand","Interval")))
  } else {
    c.out <- NULL
  }
  comp <- list(c.in=c.in,c.out=c.out,coeff=fit$coeff)

  out <- list(model=paste("ma.",type,sep=""),frc.in=as.numeric(fit$frc.in),
              frc.out=frc.out,order=c(k.demand,k.interval),components=comp)

  # Finalise output: lead time quantiles, class, and plot
  out <- id.finalise(out,"crost.ma",fit$cc[k],data,prep$tsp,h,cost,probs,qtype,
                     opt=c(w=w.opt,init=FALSE),outplot=outplot)

  return(out)

}

#-------------------------------------------------
crost.ma.fit <- function(data,k.demand,k.interval,type){
# Fit Croston-style moving averages with given orders - internal function

  n <- length(data)

  # Croston decomposition
  nzd <- which(data != 0)               # Find location on non-zero demand
  k <- length(nzd)
  z <- data[nzd]                        # Demand
  x <- c(nzd[1],nzd[2:k]-nzd[1:(k-1)])  # Intervals

  # Set coefficient
  if (type == "sba"){
    coeff <- k.interval/(k.interval+1)
  } else if (type == "sbj"){
    coeff <- (k.interval-1)/k.interval
  } else {
    coeff <- 1
  }

  # Calculate MA using filter
  zfit <- filter(z,rep(1/k.demand,k.demand),sides=2)
  zfit <- c(rep(NA,k.demand-1),zfit[!is.na(zfit)])
  xfit <- filter(x,rep(1/k.interval,k.interval),sides=2)
  xfit <- c(rep(NA,k.interval-1),xfit[!is.na(xfit)])
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
crost.ma.opt <- function(data,type,cost,nop=2,k,h=0){
# Optimisation function for Croston-style moving averages - internal function
#
# Grid search to avoid integer optimisation.

  if (nop==1){
    jmax <- 1
  } else {
    jmax <- k
  }

  err <- array(NA,c(k,jmax))
  for (i in 1:k){
    for (j in 1:jmax){
      if (nop==2){
        frc.in <- crost.ma.fit(data,i,j,type)$frc.in
      } else {
        frc.in <- crost.ma.fit(data,i,i,type)$frc.in
      }
      err[i,j] <- id.cost(data,as.numeric(frc.in),cost,h)
    }
  }

  # If there are ties, use the first in the grid (smallest interval order, then demand order)
  wopt <- which(err==min(err,na.rm=TRUE),arr.ind=TRUE)[1,]
  wopt <- as.numeric(wopt)

  if (nop==1){
    wopt <- wopt[1]
  }

  return(wopt)

}
