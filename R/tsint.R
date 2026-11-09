#-------------------------------------------------
id.finalise <- function(out,method,level,data,tsp.in,h,cost,probs,qtype,opt,outplot,
                        frc.origin=NULL,cls="tsint",extra=NULL){
  # Add lead time quantiles, stored information, class, and plot to the output of a
  # method - internal function
  #
  # Inputs:
  #   out         Output list of the method.
  #   method      Name of the function that produced the output, e.g. "crost".
  #   level       Forecast (demand rate) from the end of the sample, stored so that
  #               forecasts for any horizon can be produced without refitting.
  #   data        Time series (numeric vector), as modelled.
  #   tsp.in      Time attributes of the series, or NULL.
  #   h           Forecast horizon (lead time).
  #   cost        Cost function used for optimisation.
  #   probs       Probabilities of lead time quantiles, or NULL.
  #   qtype       Type of quantile calculation.
  #   opt         Logical vector c(w=,init=) indicating what was optimised.
  #   outplot     NULL or the name of the plot view.
  #   frc.origin  Forecasts from each in-sample origin, if these differ from frc.in
  #               (iMAPA). NULL otherwise.
  #   cls         Class of the output.
  #   extra       List of additional information to store, used by plots.
  #
  # Output:
  #   out         Object of class cls. Information needed by its methods is stored in
  #               the element fit.info, appended at the end of the list, so that the
  #               positions of the other elements are unchanged. The name fit.info does
  #               not share a prefix with existing elements that could be reached with
  #               partial matching of $ (fit$f was already ambiguous).
  
  if (is.null(frc.origin)){
    origin <- out$frc.in
  } else {
    origin <- frc.origin
  }
  
  # In-sample value of the cost function, only if something was optimised
  if (any(opt)){
    metric <- id.cost(data,origin,cost,h)
  } else {
    metric <- NULL
  }
  
  # Lead time quantiles
  if (!is.null(probs)){
    E <- id.lterrors(data,origin,h,origin=TRUE)
    out$lt <- id.ltdist(E$E,level,h,probs,qtype,E$origin)
  }
  
  info <- list(method=method,level=level,data=data,tsp=tsp.in,h=h,cost=cost,opt=opt,
               metric=metric,qtype=qtype,frc.origin=frc.origin)
  if (!is.null(extra)){
    info <- c(info,extra)
  }
  out$fit.info <- info
  class(out) <- unique(c(cls,"tsint"))
  
  if (!is.null(outplot)){
    plot(out,view=outplot)
  }
  
  return(out)
  
}

#-------------------------------------------------
id.origin <- function(x){
  # Forecasts from each in-sample origin of a fitted object - internal function
  #
  # For iMAPA these are calculated here if they were not calculated when fitting.
  
  info <- x$fit.info
  if (info$method == "imapa"){
    if (is.null(info$frc.origin)){
      return(imapa.origin(info$data,x$model.fit,info$comb))
    }
    return(info$frc.origin)
  }
  return(x$frc.in)
  
}

#-------------------------------------------------
id.label <- function(model){
  # Descriptive name of a model - internal function
  
  lbl <- switch(model,
                "croston"="Croston's method",
                "sba"="Croston's method (SBA)",
                "sbj"="Croston's method (SBJ)",
                "tsb"="TSB method",
                "sexsm"="Simple exponential smoothing",
                "ma.croston"="Croston-style moving averages",
                "ma.sba"="Croston-style moving averages (SBA)",
                "ma.sbj"="Croston-style moving averages (SBJ)",
                "imapa"="iMAPA",
                model)
  return(lbl)
  
}

#-------------------------------------------------
id.params <- function(x){
  # Parameters and initial values of a model with their names - internal function
  
  if (!is.null(x$alpha)){
    prm <- c(level=x$alpha)
    ini <- c(level=x$initial)
    lbl <- "Smoothing parameter"
  } else if (!is.null(x$order)){
    prm <- c(demand=x$order[1],interval=x$order[2])
    ini <- NULL
    lbl <- "Moving average orders"
  } else if (x$model == "tsb"){
    prm <- c(demand=x$weights[1],probability=x$weights[2])
    ini <- c(demand=x$initial[1],probability=x$initial[2])
    lbl <- "Smoothing parameters"
  } else {
    prm <- c(demand=x$weights[1],interval=x$weights[2])
    ini <- c(demand=x$initial[1],interval=x$initial[2])
    lbl <- "Smoothing parameters"
  }
  
  return(list(prm=prm,ini=ini,lbl=lbl))
  
}

#-------------------------------------------------
id.printlt <- function(lt){
  # Print lead time quantiles - internal function
  
  cat(paste0("Demand over the lead time of ",lt$h," period",ifelse(lt$h>1,"s",""),
             " (",lt$n," cumulative errors):\n"))
  tbl <- rbind("Cumulative forecast"=rep(lt$frc,length(lt$probs)),
               "Quantile"=lt$quantile,"Safety stock"=lt$ss)
  colnames(tbl) <- names(lt$quantile)
  print(round(tbl,4))
  
}

#-------------------------------------------------
#' Methods for intermittent demand forecasts
#'
#' @description Print, summarise, and extract the parameters and in-sample forecasts of
#' the forecasts produced by \code{\link{crost}}, \code{\link{tsb}},
#' \code{\link{sexsm}}, \code{\link{crost.ma}}, and \code{\link{imapa}}.
#'
#' @param x,object An object of class \code{tsint}.
#' @param ... Unused.
#'
#' @details The summary reports the characteristics of the series, the parameters and
#'   whether they were optimised, and the forecast. The in-sample value of the cost
#'   function is reported only when something was optimised, as it is the criterion of
#'   the optimisation. When lead time quantiles were requested, these are reported
#'   together with the safety stock. The series is also classified according to the
#'   PKa scheme of \code{\link{idclass}}.
#'
#' @return \code{summary} returns an object of class \code{summary.tsint}. \code{coef}
#'   returns the smoothing parameters, or the orders for \code{\link{crost.ma}}.
#'   \code{fitted} returns the in-sample forecasts \code{frc.in}. If the series was a
#'   \code{ts} object, these are returned as a \code{ts} object with the same time
#'   index, unless NA values were removed with \code{na.rm}.
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{plot.tsint}}.
#'
#' @examples
#' fit <- crost(ts.data1, h=3, type="sba", probs=c(0.9,0.95))
#' fit
#' summary(fit)
#' coef(fit)
#' head(fitted(fit))
#'
#' @name tsint-methods
#' @rdname tsint-methods
#' @export
print.tsint <- function(x,...){
  
  info <- x$fit.info
  cat(paste0(id.label(x$model),"\n"))
  prm <- id.params(x)
  opt <- info$opt
  cat(paste0(prm$lbl,": ",paste(names(prm$prm),sapply(signif(prm$prm,4),format),collapse=", "),
             ifelse(opt["w"]," (optimised)",""),"\n"))
  if (!is.null(prm$ini)){
    cat(paste0("Initial values: ",paste(names(prm$ini),sapply(signif(prm$ini,4),format),collapse=", "),
               ifelse(opt["init"]," (optimised)",""),"\n"))
  }
  if (length(x$frc.out)>0){
    cat(paste0("Forecast for h = ",length(x$frc.out),": ",format(signif(x$frc.out[1],4)),
               " per period\n"))
  }
  if (!is.null(x$lt)){
    id.printlt(x$lt)
  }
  invisible(x)
  
}

#' @rdname tsint-methods
#' @export
summary.tsint <- function(object,...){
  
  info <- object$fit.info
  y <- info$data
  ch <- id.char(y[!is.na(y)])
  if (is.na(ch[1])){
    cls <- NA
  } else if (ch[1] <= 1){
    cls <- "SES"
  } else if (ch[2] > 2-(3/2)*ch[1]){
    cls <- "SBA"
  } else {
    cls <- "Croston"
  }
  out <- list(model=object$model,n=length(y),nonzero=sum(y!=0,na.rm=TRUE),
              p=as.numeric(ch[1]),cv2=as.numeric(ch[2]),class=cls,
              params=id.params(object),opt=info$opt,cost=info$cost,
              metric=info$metric,h=info$h,frc.out=object$frc.out,lt=object$lt)
  class(out) <- "summary.tsint"
  return(out)
  
}

#' @rdname tsint-methods
#' @export
print.summary.tsint <- function(x,...){
  
  cat(paste0(id.label(x$model),"\n\n"))
  cat(paste0("Series: ",x$n," periods, ",x$nonzero," with demand.\n"))
  if (!is.na(x$p)){
    cat(paste0("Mean inter-demand interval (p): ",format(signif(x$p,4)),
               ", CV^2 of non-zero demand: ",format(signif(x$cv2,4)),
               ", PKa class: ",x$class,"\n"))
  }
  cat("\n")
  prm <- x$params
  if (any(x$opt)){
    cost <- toupper(x$cost)
    if (x$cost %in% c("msce","mace")){
      cost <- paste0(cost," over h = ",x$h)
    }
    cat(paste0("Optimised with cost function ",cost,".\n"))
  }
  cat(paste0(prm$lbl,": ",paste(names(prm$prm),sapply(signif(prm$prm,4),format),collapse=", "),
             ifelse(x$opt["w"]," (optimised)"," (fixed)"),"\n"))
  if (!is.null(prm$ini)){
    cat(paste0("Initial values: ",paste(names(prm$ini),sapply(signif(prm$ini,4),format),collapse=", "),
               ifelse(x$opt["init"]," (optimised)"," (fixed)"),"\n"))
  }
  if (!is.null(x$metric)){
    cat(paste0("In-sample ",toupper(x$cost),": ",format(signif(x$metric,4)),"\n"))
  }
  cat("\n")
  if (length(x$frc.out)>0){
    cat(paste0("Forecast for h = ",length(x$frc.out),": ",format(signif(x$frc.out[1],4)),
               " per period\n"))
  } else {
    cat("No out-of-sample forecast (h = 0).\n")
  }
  if (!is.null(x$lt)){
    id.printlt(x$lt)
  }
  invisible(x)
  
}

#' @rdname tsint-methods
#' @export
coef.tsint <- function(object,...){
  
  return(id.params(object)$prm)
  
}

#' @rdname tsint-methods
#' @export
fitted.tsint <- function(object,...){
  
  frc <- object$frc.in
  tsp.in <- object$fit.info$tsp
  if (!is.null(tsp.in)){
    frc <- ts(frc,start=tsp.in[1],frequency=tsp.in[3])
  }
  return(frc)
  
}

#-------------------------------------------------
#' Plot intermittent demand forecasts
#'
#' @description Plot the forecasts produced by \code{\link{crost}}, \code{\link{tsb}},
#' \code{\link{sexsm}}, and \code{\link{crost.ma}}, in two views: per period or over
#' the lead time.
#'
#' @param x An object of class \code{tsint}.
#' @param view The view to plot:
#'   \itemize{
#'   \item \code{"period"}: the series, with the in-sample and out-of-sample demand
#'   rate. When lead time quantiles are available, each quantile is shown divided by
#'   the lead time, spread over the forecast horizon. This places the quantile next to
#'   the forecast, but it is not a per-period quantile.
#'   \item \code{"leadtime"}: the rolling cumulative demand over windows of \code{h}
#'   periods, plotted at the end of each window, and the cumulative forecasts made at
#'   the start of each window. When lead time quantiles are available, the in-sample
#'   cumulative forecasts plus the empirical error quantiles are shown as well, and the
#'   cumulative forecast and quantiles for the next \code{h} periods are marked at the
#'   end of the forecast horizon. This is the view in which the predictive distribution
#'   is defined. It needs \code{h} of at least 1.
#'   }
#' @param legend If \code{TRUE}, a legend is added when it is needed.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}, for example
#'   \code{main}, \code{xlab}, \code{ylab}, and \code{ylim}.
#'
#' @return No return value, called for the plot.
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{crost}}, \code{\link{plot.imapa}}.
#'
#' @examples
#' fit <- crost(ts.data1, h=3, type="sba", probs=c(0.9,0.95))
#' plot(fit)
#' plot(fit, view="leadtime")
#' plot(fit, view="leadtime", main="SBA", ylab="Demand over 3 periods")
#'
#' # The lead time view is also available without quantiles
#' plot(tsb(ts.data1, h=6), view="leadtime")
#'
#' @export
plot.tsint <- function(x,view=c("period","leadtime"),legend=TRUE,...){
  
  view <- id.match(view,c("period","leadtime"),"view")
  info <- x$fit.info
  if (view == "period"){
    id.plot.period(info$data,info$tsp,x$frc.in,x$frc.out,x$lt,id.label(x$model),legend,...)
  } else {
    id.plot.leadtime(info$data,info$tsp,id.origin(x),info$level,info$h,x$lt,
                     id.label(x$model),legend,...)
  }
  invisible(NULL)
  
}

#-------------------------------------------------
id.time <- function(n,h,tsp.in){
  # Time index of the in-sample and forecast periods - internal function
  
  if (is.null(tsp.in)){
    tt <- 1:(n+h)
  } else {
    tt <- seq(tsp.in[1],by=1/tsp.in[3],length.out=n+h)
  }
  return(tt)
  
}

#-------------------------------------------------
id.plot.period <- function(y,tsp.in,frc.in,frc.out,lt=NULL,main="",legend=TRUE,...){
  # Per-period plot of the series and the forecasts - internal function
  
  n <- length(y)
  h <- length(frc.out)
  tt <- id.time(n,h,tsp.in)
  tin <- tt[1:n]
  tout <- tt[n+seq_len(h)]
  ymax <- max(c(y,frc.in,frc.out,lt$quantile/lt$h),na.rm=TRUE)
  args <- list(x=tin,y=y,type="l",xlim=range(tt),xlab="Period",ylab="",main=main,
               xaxs="i",yaxs="i",ylim=c(0,ymax*1.1))
  args <- modifyList(args,list(...))
  do.call(plot,args)
  lines(tin[which(y>0)],y[which(y>0)],type="p",pch=20)
  lines(tin,frc.in,col="red")
  if (h>0){
    lines(tout,frc.out,col="red",lwd=2)
    if (h==1){
      points(tout,frc.out,col="red",pch=20)
    }
  }
  if (!is.null(lt)){
    nq <- length(lt$quantile)
    for (i in 1:nq){
      lines(tout[1:lt$h],rep(lt$quantile[i]/lt$h,lt$h),col="blue",lty=i+1,lwd=2)
    }
    if (legend == TRUE){
      graphics::legend("topleft",c("Forecast",paste0("Lead time quantile ",names(lt$quantile)," / ",lt$h)),
                       col=c("red",rep("blue",nq)),lty=c(1,1+(1:nq)),lwd=c(2,rep(2,nq)),
                       cex=1,bg="white")
    }
  }
  
}

#-------------------------------------------------
id.plot.leadtime <- function(y,tsp.in,frc.origin,frc.out,h,lt=NULL,main="",legend=TRUE,...){
  # Lead time plot of the cumulative demand and forecasts - internal function
  
  if (h<1){
    stop("The lead time view needs h of at least 1.")
  }
  n <- length(y)
  if (n<h){
    stop("The series is shorter than the lead time.")
  }
  tt <- id.time(n,h,tsp.in)
  idx <- 1:(n-h+1)
  xin <- tt[idx+h-1]
  ya <- as.numeric(stats::filter(y,rep(1,h),sides=1))[idx+h-1]
  # All methods produce a constant forecast over the horizon, so the cumulative
  # forecast from each origin is h times the forecast made at that origin.
  fa <- h*frc.origin[idx]
  fout <- h*frc.out[1]
  if (!is.null(lt) && lt$n>0){
    q <- as.numeric(quantile(lt$errors,lt$probs,type=lt$qtype,names=FALSE))
    qa <- sapply(q,function(qi){pmax(fa+qi,0)})
    qa <- matrix(qa,ncol=length(q))
  } else {
    q <- qa <- NULL
  }
  ymax <- max(c(ya,fa,fout,qa,lt$quantile),na.rm=TRUE)
  args <- list(x=xin,y=ya,type="l",xlim=c(min(xin),tt[n+h]),xlab="Period",
               ylab=paste0("Demand over ",h," period",ifelse(h>1,"s","")),
               main=main,yaxs="i",ylim=c(0,ymax*1.1))
  args <- modifyList(args,list(...))
  do.call(plot,args)
  abline(v=tt[n],col="grey",lty=3)
  lines(xin,fa,col="red")
  if (!is.null(qa)){
    for (i in 1:ncol(qa)){
      lines(xin,qa[,i],col="blue",lty=i+1)
    }
  }
  points(tt[n+h],fout,col="red",pch=19)
  if (!is.null(lt)){
    points(rep(tt[n+h],length(lt$quantile)),lt$quantile,col="blue",pch=17)
  }
  if (legend == TRUE){
    lgd <- c("Cumulative demand","Cumulative forecast")
    lcol <- c("black","red")
    llty <- c(1,1)
    if (!is.null(lt)){
      lgd <- c(lgd,paste0("Quantile ",names(lt$quantile)))
      lcol <- c(lcol,rep("blue",length(lt$quantile)))
      llty <- c(llty,1+seq_along(lt$quantile))
    }
    graphics::legend("topleft",lgd,col=lcol,lty=llty,cex=1,bg="white")
  }
  
}

#-------------------------------------------------
#' Empirical quantiles of the demand over the lead time
#'
#' @description Calculate the empirical quantiles of the demand over the lead time from
#' the cumulative in-sample forecast errors, either for a model fitted with the
#' functions of the package, for any lead time and probabilities without refitting it,
#' or for any forecast that is constant over the horizon.
#'
#' @param data A model fitted with \code{\link{crost}}, \code{\link{tsb}},
#'   \code{\link{sexsm}}, \code{\link{crost.ma}}, or \code{\link{imapa}}.
#'   Alternatively, a time series, which can be a vector, a \code{ts} object, or a data
#'   frame, in which case only the first column is used.
#' @param frc.in In-sample forecasts, of the same length as \code{data}, where
#'   \code{frc.in[t]} is the forecast for period \code{t} made at the end of period
#'   \code{t-1}. Periods without a forecast should be \code{NA}. Not used when
#'   \code{data} is a fitted model.
#' @param frc.out Out-of-sample forecast from the end of the sample. Only the first
#'   value is used. Not used when \code{data} is a fitted model.
#' @param h Lead time, see the section "Lead time quantiles" in \code{\link{crost}}.
#'   When \code{data} is a fitted model, its horizon is used if \code{h} is
#'   \code{NULL}.
#' @param probs Probabilities of the quantiles.
#' @param qtype Type of empirical quantile calculation, passed to
#'   \code{\link[stats]{quantile}}.
#'
#' @details This is the calculation used by the forecasting functions of the package
#'   when \code{probs} is provided. With a fitted model, the stored series and in-sample
#'   forecasts are used, so the lead time and the probabilities can be changed without
#'   optimising the model again. Note that the parameters of a model optimised with the
#'   cost functions \code{"msce"} or \code{"mace"} depend on the horizon used when
#'   fitting it. For iMAPA, the forecasts from each in-sample origin are calculated if
#'   they were not calculated when fitting, which can take some time.
#'
#'   With a time series and forecasts produced otherwise, the forecast made at each
#'   origin is assumed to be constant over the next \code{h} periods, as is the case for
#'   all methods in the package, so that the cumulative forecast from origin \code{t-1}
#'   is \code{h*frc.in[t]}.
#'
#' @return A list containing:
#' \item{h}{Lead time.}
#' \item{probs}{Probabilities.}
#' \item{frc}{Cumulative forecast over the lead time.}
#' \item{quantile}{Quantiles of the demand over the lead time, i.e., the order-up-to
#'   level or the reorder point.}
#' \item{ss}{Safety stock, i.e., the quantiles minus the cumulative forecast.}
#' \item{errors}{In-sample cumulative errors.}
#' \item{origin}{Origin of each cumulative error, i.e., the period at the end of which
#'   the forecast was made, so that the error covers periods \code{origin+1} to
#'   \code{origin+h}. Origin 0 is the start of the sample. Periods are counted from the
#'   first observation, also for \code{ts} objects.}
#' \item{n}{Number of cumulative errors.}
#' \item{qtype}{Type of quantile calculation.}
#'
#' @references
#' Saoud, P., Kourentzes, N., and Boylan, J. E. (2022). Approximations for the lead time
#' variance: A forecasting and inventory evaluation. \emph{Omega}, 110, 102614.
#' \doi{10.1016/j.omega.2022.102614}
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{crost}}, \code{\link{forecast.tsint}}.
#'
#' @examples
#' # Different lead times and probabilities for a fitted model, without refitting
#' fit <- crost(ts.data1, type="sba")
#' lt.dist(fit, h=3)
#' lt.dist(fit, h=6, probs=0.99)
#'
#' # Same result as crost(ts.data1, h=3, probs=c(0.9,0.95))$lt
#' fit <- crost(ts.data1, h=3)
#' lt.dist(ts.data1, fit$frc.in, fit$frc.out, h=3)
#'
#' # A simple moving average of the last 12 periods as the forecast
#' y <- as.numeric(ts.data1)
#' frc <- c(rep(NA,12), as.numeric(stats::filter(y, rep(1/12,12), sides=1))[12:(length(y)-1)])
#' lt.dist(y, frc, mean(tail(y,12)), h=3, probs=0.95)
#'
#' @export lt.dist
lt.dist <- function(data,frc.in=NULL,frc.out=NULL,h=NULL,probs=c(0.9,0.95),qtype=7){
  
  if (inherits(data,"tsint")){
    # Fitted model
    info <- data$fit.info
    if (is.null(h)){
      h <- info$h
    }
    y <- info$data
    frc.in <- id.origin(data)[1:length(y)]
    frc.out <- info$level
  } else {
    y <- id.prep(data)$y
    if (is.null(frc.in) || is.null(frc.out) || is.null(h)){
      stop("Inputs frc.in, frc.out, and h are needed when data is a time series.")
    }
  }
  h <- id.h(h)
  if (h<1){
    stop("Input h must be at least 1.")
  }
  probs <- id.probs(probs,h)
  if (length(frc.in) != length(y)){
    stop("Inputs data and frc.in must have the same length.")
  }
  E <- id.lterrors(y,as.numeric(frc.in),h,origin=TRUE)
  
  return(id.ltdist(E$E,as.numeric(frc.out)[1],h,probs,qtype,E$origin))
  
}