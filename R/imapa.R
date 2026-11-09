#' iMAPA: MAPA for intermittent demand
#'
#' @description Forecast intermittent demand time series by modelling them at multiple
#' temporal aggregation levels and combining the forecasts, selecting between Croston's
#' method, SBA, and SES at each level.
#'
#' @inheritParams crost
#' @param w Smoothing parameters for Croston's method and SBA. If \code{w == NULL} then
#'   the parameters are optimised. If \code{w} is a single parameter, then the same is
#'   used for smoothing both the demand and the intervals. If two parameters are
#'   provided, then the second is used to smooth the intervals. SES is always optimised.
#' @param minimumAL Lowest aggregation level to use.
#' @param maximumAL Highest aggregation level to use. If \code{NULL}, the larger of the
#'   maximum inter-demand interval of the series and its sampling frequency is used,
#'   e.g., at least 12 for monthly data. The frequency is taken from \code{ts} objects
#'   only, so for other inputs the maximum interval is used. The periods before the
#'   first demand count as an interval, as in \code{\link{crost.decomp}}. If the
#'   result is lower than \code{minimumAL}, then \code{minimumAL} is used.
#'   Aggregation levels with fewer than four aggregated observations are not used,
#'   whatever the value of \code{maximumAL}.
#' @param comb Combination operator, \code{"mean"} or \code{"median"}.
#' @param init.opt If \code{TRUE}, the initial values of Croston's method and SBA are
#'   optimised. This does not affect SES.
#' @param paral Use parallel processing: \code{0} for no, \code{1} for yes using an
#'   initialised cluster provided in \code{cl}, and \code{2} for yes, initialising a
#'   cluster with all available cores. The workers load the installed package, so it
#'   must be installed at the same version as the one in use; this is checked, and an
#'   error is given otherwise. In particular, parallel processing does not work when
#'   the package is loaded with \code{devtools::load_all()}.
#' @param outplot Plot of the forecast:
#'   \itemize{
#'   \item \code{0} or \code{FALSE}: no plot;
#'   \item \code{1}, \code{TRUE}, or \code{"period"}: time series and combined forecast;
#'   \item \code{2} or \code{"levels"}: time series and the forecasts of all aggregation
#'   levels;
#'   \item \code{3} or \code{"selection"}: summary of the models selected;
#'   \item \code{4} or \code{"selection.detail"}: detailed plot of the models selected,
#'   with the classification of the series at each aggregation level;
#'   \item \code{"leadtime"}: the lead time view, see \code{\link{plot.tsint}}.
#'   }
#'   All plots are also available afterwards with \code{\link{plot.imapa}}.
#' @param model.fit Optional input with model types and parameters. This can be the
#'   \code{model.fit} output of this function, or a model previously fitted with this
#'   function, in which case its combination operator is also used, unless \code{comb}
#'   is given. If used, it overrides other model settings, and no classification or
#'   optimisation is performed. This can be used to apply a fitted model to new data,
#'   for example when new observations become available. The same is available with
#'   \code{\link{forecast.tsint}}.
#' @param cl Cluster to use when \code{paral=1}, created with
#'   \code{parallel::makeCluster}. If \code{NULL}, a cluster named \code{cl} in the
#'   global environment is used, for compatibility with earlier versions.
#'
#' @details Temporal aggregation sums the series into non-overlapping buckets of several
#'   periods. At higher aggregation levels there are fewer zeros, so the series becomes
#'   easier to forecast, but the best level differs between series and is not known in
#'   advance. iMAPA avoids this choice by using all levels from \code{minimumAL} to
#'   \code{maximumAL} (Petropoulos and Kourentzes, 2015). At each level, the first
#'   observations are dropped where needed, so that all buckets are complete; the
#'   aggregated series is classified with the PKa scheme of \code{\link{idclass}}; and
#'   Croston's method, SBA, or SES is fitted accordingly, with parameters optimised using
#'   MAR. The forecast of each level is divided equally over the periods of a bucket, and
#'   the forecasts of all levels are combined. Levels with fewer than four aggregated
#'   observations, or fewer than two non-zero values, are not used.
#'
#'   The forecast is a demand rate, constant over the horizon. The in-sample forecast
#'   \code{frc.in} of each level is produced at the aggregated frequency, so within each
#'   bucket it is the forecast made at the start of the bucket.
#'
#' @section Lead time quantiles:
#' The quantiles are calculated as described in \code{\link{crost}}, from the cumulative
#' errors over \code{h} periods. Because the in-sample forecasts of the aggregation
#' levels are made at the start of each bucket, rather than at every period, the
#' cumulative errors are calculated from forecasts made at every in-sample origin, using
#' the fitted models of each level with fixed parameters. Levels that do not yet have
#' enough observations at an origin are left out of the combination at that origin.
#' This requires forecasts from every origin and every level, so it is slower than the
#' other methods; \code{paral} can be used to speed it up. Unlike the other methods, the
#' cumulative cost functions MSCE and MACE are not offered, as temporal aggregation
#' already targets the demand over multiple periods.
#'
#' @return An object of class \code{imapa}, which is a list containing:
#' \item{frc.in}{In-sample demand rate.}
#' \item{frc.out}{Out-of-sample demand rate.}
#' \item{summary}{An array containing information for each aggregation level:
#'   \code{AL}, the aggregation level; \code{n}, the number of observations of the
#'   aggregated series; \code{p}, the average inter-demand interval; \code{cv2}, the
#'   squared coefficient of variation of the non-zero demand; \code{model}, the
#'   selected model, where 1 is Croston, 2 is SBA, and 3 is SES; and \code{use}, which
#'   is 0 if the aggregation level is not used.}
#' \item{model.fit}{Parameters and initialisation values of the fitted model at each
#'   aggregation level, with aggregation level and model information.}
#' \item{lt}{Only when \code{probs} is provided. See \code{\link{crost}}.}
#' \item{fit.info}{Information used by the methods of the object, such as the series,
#'   the forecast from the end of the sample, and the cost function. Not intended for
#'   direct use.}
#' The object can be used with \code{print}, \code{summary}, \code{plot}, \code{coef},
#' \code{fitted}, and \code{forecast}.
#'
#' @references
#' Petropoulos, F., and Kourentzes, N. (2015). Forecast combinations for intermittent
#' demand. \emph{Journal of the Operational Research Society}, 66(6), 914--924.
#' \doi{10.1057/jors.2014.62}
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{plot.imapa}}, \code{\link{idclass}}, \code{\link{crost}}.
#'
#' @examples
#' # iMAPA with a plot of the combined forecast
#' fit <- imapa(ts.data1, outplot=1)
#' summary(fit)
#'
#' # Forecasts of each aggregation level and the models selected
#' plot(fit, view="levels")
#' plot(fit, view="selection.detail")
#'
#' # Fewer aggregation levels, combined with the median
#' imapa(ts.data1, maximumAL=12, comb="median")$frc.out
#'
#' # Reuse the fitted models, for example on an updated series
#' imapa(ts.data1, model.fit=fit)$frc.out
#'
#' # Quantiles of the demand over a lead time of 3 periods
#' fit <- imapa(ts.data1, h=3, maximumAL=12, probs=c(0.9,0.95), outplot="leadtime")
#' fit$lt$quantile
#'
#' \donttest{
#' # Parallel processing with a cluster
#' cl <- parallel::makeCluster(2)
#' imapa(ts.data1, paral=1, cl=cl)$frc.out
#' parallel::stopCluster(cl)
#' }
#'
#' @export imapa
imapa <- function(data,h=10,w=NULL,minimumAL=1,maximumAL=NULL,comb=c("mean","median"),
                  init.opt=c(TRUE,FALSE),paral=c(0,1,2),outplot=c(0,1,2,3,4),
                  model.fit=NULL,na.rm=c(FALSE,TRUE),probs=NULL,qtype=7,cl=NULL){
  
  # Reuse a fitted model
  if (!is.null(model.fit) && inherits(model.fit,"imapa")){
    if (missing(comb)){
      comb <- model.fit$fit.info$comb
    }
    model.fit <- model.fit$model.fit
  }
  
  # Defaults
  h <- id.h(h)
  comb <- id.match(comb,c("mean","median"),"comb")
  paral <- paral[1]
  if (!(paral %in% c(0,1,2))){
    stop("Input paral must be one of: 0, 1, 2.")
  }
  outplot <- id.imapa.outplot(outplot)
  init.opt <- init.opt[1]
  na.rm <- na.rm[1]
  probs <- id.probs(probs,h)
  if (identical(outplot,"leadtime") && h<1){
    stop("The lead time view needs h of at least 1.")
  }
  
  # Sampling frequency, used for the default maximumAL. Taken before preparing the
  # data, as the time attributes are dropped when NA values are removed.
  if (is.ts(data)){
    freq <- floor(frequency(data))
  } else {
    freq <- 1
  }
  
  # Prepare data
  prep <- id.prep(data,na.rm)
  data <- prep$y
  n <- length(data)
  
  # Check number of non-zero values - need to have at least two
  if (sum(data!=0,na.rm=TRUE)<2){
    stop("Need at least two non-zero values to model time series.")
  }
  
  # Setup parallel processing if required
  if (paral != 0){
    clout <- id.cluster(paral,cl)
    cl <- clout$cl
    if (clout$created == TRUE){
      on.exit(parallel::stopCluster(cl))
    }
  }
  
  # Find interval alpha
  if (!is.null(w)){
    w.in <- w[length(w)]
  } else {
    w.in <- NULL
  }
  
  # If model.fit is provided override minimum and maximum AL
  if (!is.null(model.fit)){
    minimumAL <- min(model.fit[1,])
    maximumAL <- max(model.fit[1,])
  }
  
  # If no maximumAL use the maximum of the maximum interval and the sampling frequency
  if (is.null(maximumAL)){
    maximumAL <- max(c(crost.decomp(data,init="naive")$interval,freq,minimumAL))
  }
  
  # Check minimumAL and maximumAL
  if (minimumAL>maximumAL){
    stop("minimumAL must be smaller or equal to maximumAL.")
  }
  
  # Aggregate time series
  yaggr <- id.aggr(data,minimumAL:maximumAL)
  
  # Model each aggregation level
  k <- length(yaggr$out)
  ychar <- array(NA,c(6,k),dimnames=list(c("AL","n","p","cv2","model","use"),names(yaggr$out)))
  fit <- array(NA,c(6,k),dimnames=list(c("AL","model","a1","a2","initial.z","initial.x"),names(yaggr$out)))
  f.in <- array(NA,c(k,n),dimnames=list(names(yaggr$out)))
  f.out <- array(NA,c(k,h),dimnames=list(names(yaggr$out)))
  
  # Forecast calculation
  if (paral != 0){  # Parallel run
    Fc_par <- parallel::clusterApplyLB(cl,1:k,imapa.loop,yaggr=yaggr,minimumAL=minimumAL,w=w,
                                       w.in=w.in,init.opt=init.opt,n=n,h=h,model.fit=model.fit)
  } else {          # Serial run
    Fc_par <- vector("list",k)
    for (i in 1:k){
      Fc_par[[i]] <- imapa.loop(i,yaggr,minimumAL,w,w.in,init.opt,n,h,model.fit)
    }
  }
  
  # Distribute parallel output
  Fc_par <- do.call(rbind,Fc_par)
  ychar[] <- t(Fc_par[,1:6])
  fit[] <- t(Fc_par[,7:12])
  if (h>0){
    f.out[] <- Fc_par[,13:(12+h)]
  }
  f.in[] <- Fc_par[,(13+h):length(Fc_par[1,])]
  
  # Combine forecasts
  use <- which(ychar[6,]==1)
  if (length(use)==0){
    stop("No aggregation level has enough observations to be modelled.")
  }
  if (length(use)>1){
    # If multiple aggregation levels are used
    if (comb=="median"){ # median
      frc.in <- apply(f.in[use,],2,"median")
      frc.out <- apply(f.out[use,,drop=FALSE],2,"median")
    } else { # mean
      frc.in <- colMeans(f.in[use,])
      frc.out <- colMeans(f.out[use,,drop=FALSE])
    }
  } else {
    # Single aggregation level
    frc.in <- as.numeric(f.in[use,])
    frc.out <- as.numeric(f.out[use,])
  }
  
  out <- list(frc.in=frc.in,frc.out=frc.out,summary=ychar,model.fit=fit)
  
  # Forecasts from each in-sample origin, needed for the lead time quantiles
  if (!is.null(probs) || identical(outplot,"leadtime")){
    if (paral == 0){
      cl <- NULL
    }
    frc.origin <- imapa.origin(data,fit,comb,cl)
  } else {
    frc.origin <- NULL
  }
  
  # Finalise output: lead time quantiles, class, and plot
  # The forecast from the end of the sample is the forecast from the last origin
  if (!is.null(frc.origin)){
    level <- frc.origin[n+1]
    frc.origin <- frc.origin[1:n]
  } else {
    level <- imapa.origin.loop(n,data,fit,comb)
  }
  out <- id.finalise(out,"imapa",level,data,prep$tsp,h,"mar",probs,qtype,opt=c(w=FALSE,init=FALSE),
                     outplot=NULL,frc.origin=frc.origin,cls="imapa",
                     extra=list(f.in=f.in,f.out=f.out,comb=comb))
  if (!is.null(outplot)){
    plot(out,view=outplot)
  }
  
  return(out)
  
}

#-------------------------------------------------
id.imapa.outplot <- function(outplot){
  # Interpret the outplot input of imapa - internal function
  #
  # Output:
  #   NULL if no plot is requested, otherwise the plot view (1 to 4, or "leadtime").
  
  outplot <- outplot[1]
  if (is.logical(outplot)){
    outplot <- as.numeric(outplot)
  }
  if (is.numeric(outplot)){
    if (is.na(outplot) || !(outplot %in% 0:4)){
      stop("Input outplot must be one of: 0, 1, 2, 3, 4, TRUE, FALSE, period, levels, selection, selection.detail, leadtime.")
    }
    if (outplot == 0){
      return(NULL)
    }
    return(outplot)
  }
  outplot <- id.match(outplot,c("period","levels","selection","selection.detail","leadtime"),"outplot")
  if (outplot != "leadtime"){
    outplot <- match(outplot,c("period","levels","selection","selection.detail"))
  }
  return(outplot)
  
}

#-------------------------------------------------
imapa.loop <- function(i,yaggr,minimumAL,w,w.in,init.opt,n,h,model.fit){
  # Forecast for each individual aggregation level - internal function
  
  ytemp <- as.vector(yaggr$out[[i]])
  ychartemp <- array(NA,c(1,6))
  
  # Get time series characteristics
  ychartemp[1] <- minimumAL + i - 1
  ychartemp[2] <- length(ytemp)
  ychartemp[6] <- ychartemp[2]>=4
  
  # Check if at this aggregation level there are at least two non-zero observations
  if (ychartemp[6]==1 && sum(ytemp!=0)<2){
    ychartemp[6] <- 0
  }
  
  if (ychartemp[6]==1){
    if (!is.null(model.fit)){
      # Select prefit model
      fit <- model.fit[,i]
      if (!is.na(fit[2])){
        chartemp <- idclass(ytemp,type="PKa",a.in=0.1,outplot="none")
        ychartemp[3] <- chartemp$p
        ychartemp[4] <- chartemp$cv2
        ychartemp[5] <- fit[2]
      } else {
        ychartemp[6] <- 0
      }
    } else {
      # Identify model
      chartemp <- idclass(ytemp,type="PKa",a.in=w.in,outplot="none")
      ychartemp[3] <- chartemp$p
      ychartemp[4] <- chartemp$cv2
      ychartemp[5] <- which(chartemp$summary==1)
    }
  }
  
  # Fit model and produce forecasts
  if (ychartemp[6]==1){
    # Check if model.fit is given or optimise models
    if (!is.null(model.fit)){
      # Use model.fit
      if (fit[2] == 1){
        # Croston
        ftemp <- crost(ytemp,h=1,w=fit[3:4],init=fit[5:6],init.opt=FALSE)
      } else if (fit[2] == 2){
        # SBA
        ftemp <- crost(ytemp,h=1,w=fit[3:4],init=fit[5:6],init.opt=FALSE,type="sba")
      } else {
        # SES
        ftemp <- sexsm(ytemp,h=1,w=fit[3],init=fit[5],init.opt=FALSE)
      }
      p <- fit
    } else {
      # Fit new models
      if (ychartemp[5]==1){
        # Croston
        ftemp <- crost(ytemp,h=1,w=w,init.opt=init.opt)
        p <- c(minimumAL+i-1,1,ftemp$weights,ftemp$initial)
      } else if (ychartemp[5]==2){
        # SBA
        ftemp <- crost(ytemp,h=1,w=w,init.opt=init.opt,type="sba")
        p <- c(minimumAL+i-1,2,ftemp$weights,ftemp$initial)
      } else {
        # SES
        ftemp <- sexsm(ytemp,h=1)
        p <- c(minimumAL+i-1,3,ftemp$alpha,NA,ftemp$initial,NA)
      }
    }
    # Disaggregate forecasts
    ftemp.out <- array(NA,c(1,h))
    ftemp.out[] <- ftemp$frc.out/ychartemp[1]
    fin <- as.vector(matrix(rep(ftemp$frc.in/ychartemp[1],ychartemp[1]),nrow=ychartemp[1],byrow=TRUE))
    ftemp.in <- array(NA,c(1,n))
    ftemp.in[yaggr$idx[[i]][1]:n] <- fin
  } else {
    # No model - not enough sample
    ftemp.out <- array(NA,c(1,h))
    ftemp.in <- array(NA,c(1,n))
    p <- c(minimumAL+i-1,rep(NA,5))
  }
  p <- matrix(p,ncol=6)
  
  return(cbind(ychartemp,p,ftemp.out,ftemp.in))
  
}

#-------------------------------------------------
imapa.origin <- function(data,model.fit,comb,cl=NULL){
  # Forecasts of iMAPA from each in-sample origin - internal function
  #
  # Inputs:
  #   data        Time series (numeric vector).
  #   model.fit   Fitted models of each aggregation level, as returned by imapa.
  #   comb        Combination operator.
  #   cl          Cluster for parallel processing, or NULL.
  #
  # Output:
  #   frc.origin  Vector of length n+1, where frc.origin[t] is the forecast made at the end
  #               of period t-1, NA where no aggregation level can be used. The last
  #               element is the forecast from the end of the sample. Forecasts from all
  #               origins are calculated, so that they can be reused for any lead time.
  #
  # Notes:
  # The in-sample forecasts of each aggregation level are made at the start of each
  # bucket, so they cannot be used for cumulative errors from every origin. Instead, at
  # each origin the series up to that point is aggregated and forecasted with the fitted
  # models, keeping their parameters fixed.
  
  n <- length(data)
  tv <- 2:(n+1)
  if (is.null(cl)){
    fo <- sapply(tv-1,imapa.origin.loop,y=data,model.fit=model.fit,comb=comb)
  } else {
    fo <- unlist(parallel::clusterApplyLB(cl,tv-1,imapa.origin.loop,y=data,
                                          model.fit=model.fit,comb=comb))
  }
  frc.origin <- rep(NA,n+1)
  frc.origin[tv] <- fo
  
  return(frc.origin)
  
}

#-------------------------------------------------
imapa.origin.loop <- function(o,y,model.fit,comb){
  # Combined forecast of iMAPA made at origin o, with fixed models - internal function
  
  y <- y[1:o]
  k <- ncol(model.fit)
  f <- rep(NA,k)
  for (j in 1:k){
    m <- model.fit[2,j]
    if (is.na(m)){
      next
    }
    al <- model.fit[1,j]
    # Same requirements as imapa.loop: at least 4 aggregated observations and 2 non-zero
    if (o < 4*al){
      next
    }
    slost <- o %% al
    yk <- colSums(matrix(y[(slost+1):o],nrow=al))
    if (sum(yk!=0)<2){
      next
    }
    if (m == 1){
      fk <- crost.fit(yk,model.fit[3:4,j],model.fit[5:6,j],"croston")$cc
    } else if (m == 2){
      fk <- crost.fit(yk,model.fit[3:4,j],model.fit[5:6,j],"sba")$cc
    } else {
      fk <- sexsm.fit(yk,model.fit[3,j],model.fit[5,j])$fit
    }
    f[j] <- fk[length(fk)]/al
  }
  f <- f[!is.na(f)]
  if (length(f) == 0){
    return(NA)
  }
  if (comb == "median"){
    return(median(f))
  } else {
    return(mean(f))
  }
  
}

#-------------------------------------------------
#' Plot and summarise iMAPA forecasts
#'
#' @description Plot and summarise the forecasts produced by \code{\link{imapa}}.
#'
#' @param x,object An object of class \code{imapa}.
#' @param view The plot to produce:
#'   \itemize{
#'   \item \code{1} or \code{"period"}: time series and combined forecast;
#'   \item \code{2} or \code{"levels"}: time series and the forecasts of all aggregation
#'   levels;
#'   \item \code{3} or \code{"selection"}: summary of the models selected;
#'   \item \code{4} or \code{"selection.detail"}: detailed plot of the models selected,
#'   with the classification of the series at each aggregation level;
#'   \item \code{"leadtime"}: the lead time view, as described in
#'   \code{\link{plot.tsint}}. If the forecasts from each in-sample origin were not
#'   calculated by \code{\link{imapa}}, they are calculated here, which can take some
#'   time.
#'   }
#' @param legend If \code{TRUE}, a legend is added when it is needed.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}, for example
#'   \code{main}. For \code{print} and \code{summary}, unused.
#'
#' @return \code{plot} has no return value. \code{summary} returns an object of class
#'   \code{summary.imapa}, and \code{coef} returns the \code{model.fit} of each
#'   aggregation level.
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{imapa}}.
#'
#' @examples
#' fit <- imapa(ts.data1, h=3, maximumAL=12)
#' fit
#' summary(fit)
#' plot(fit)
#' plot(fit, view="levels")
#' plot(fit, view="selection")
#' plot(fit, view="leadtime")
#'
#' @export
plot.imapa <- function(x,view=1,legend=TRUE,...){
  
  view <- id.imapa.outplot(view)
  if (is.null(view)){
    return(invisible(NULL))
  }
  info <- x$fit.info
  data <- info$data
  n <- length(data)
  ychar <- x$summary
  k <- ncol(ychar)
  
  if (identical(view,"leadtime")){
    id.plot.leadtime(data,info$tsp,id.origin(x)[1:n],info$level,info$h,x$lt,"iMAPA",legend,...)
  } else if (view == 1){
    id.plot.period(data,info$tsp,x$frc.in,x$frc.out,x$lt,"iMAPA",legend,...)
  } else if (view == 2){
    # Detail forecast
    h <- length(x$frc.out)
    tt <- id.time(n,h,info$tsp)
    tin <- tt[1:n]
    tout <- tt[n+seq_len(h)]
    use <- which(ychar[6,]==1)
    cmp <- rainbow(length(use),start=2/6,end=4/6)
    args <- list(x=tin,y=data,type="l",xlim=range(tt),xlab="Period",ylab="",main="iMAPA",
                 xaxs="i",yaxs="i",ylim=c(0,max(data,na.rm=TRUE)*1.1),lwd=2)
    args <- modifyList(args,list(...))
    do.call(plot,args)
    lines(tin[which(data>0)],data[which(data>0)],type="p",pch=20,lwd=2)
    for (i in seq_along(use)){
      lines(tin,info$f.in[use[i],],col=cmp[i])
      if (h>0){
        lines(tout,info$f.out[use[i],],col=cmp[i])
      }
    }
    lines(tin,x$frc.in,col="red",lwd=2)
    if (h>0){
      lines(tout,x$frc.out,col="red",lwd=2)
    }
    if (legend == TRUE){
      if (length(use)>1){
        graphics::legend("topleft",c(paste0("AL",ychar[1,use[1]]),paste0("AL",ychar[1,use[length(use)]]),"Combined"),
                         col=c(cmp[1],cmp[length(use)],"red"),lty=1,lwd=c(1,1,2),cex=1,bg="white")
      } else {
        graphics::legend("topleft",c(paste0("AL",ychar[1,use[1]]),"Combined"),
                         col=c(cmp[1],"red"),lty=1,lwd=c(1,2),cex=1,bg="white")
      }
    }
  } else if (view == 3){
    # Model selection summary
    xx <- c(0.94,1.5)
    yy <- c(0,1)
    args <- list(x=0,y=0,type="n",xlim=xx,ylim=yy,xaxs="i",yaxs="i",xlab="p",
                 ylab=parse(text="CV^2"),main="iMAPA model selection",xaxt="n",yaxt="n")
    args <- modifyList(args,list(...))
    do.call(plot,args)
    id.classarea("PKA")
    axis(1,at=c(1,4/3),labels=c("1","4/3"))
    axis(2,at=c(0,0.5),labels=c("0","1/2"))
    text(xx[2],1,paste("SBA: ",sum(ychar[5,]==2,na.rm=TRUE),sep=""),adj=c(1.2,1.2))
    text(1,0,paste("Croston: ",sum(ychar[5,]==1,na.rm=TRUE),sep=""),adj=c(-0.2,-1.2))
    polygon(c(0,1,1,0),c(0,0,yy[2],yy[2]),col=gray(0.6),border=NA)
    text(0.97,0,paste("SES: ",sum(ychar[5,]==3,na.rm=TRUE),sep=""),adj=c(-0.2,0.2),srt=90)
  } else {
    # Model selection detail
    use <- which(ychar[6,]==1)
    cmp <- rainbow(length(use),start=2/6,end=4/6)
    p <- ychar[3,use]
    v <- ychar[4,use]
    v[is.na(v)] <- 0
    xmax <- max(c(max(p)+diff(range(p))*0.1),1.5)
    xmin <- 1-(xmax-1)*0.1
    xx <- c(xmin,xmax)
    yy <- c(0,max(c(max(v)+diff(range(v))*0.1),1.5))
    args <- list(x=0,y=0,type="n",xaxs="i",yaxs="i",xlim=xx,ylim=yy,xlab="p",
                 ylab=parse(text="CV^2"),main="iMAPA model selection")
    args <- modifyList(args,list(...))
    do.call(plot,args)
    id.classarea("PKA")
    polygon(c(0,1,1,0),c(0,0,yy[2],yy[2]),col=gray(0.4),border=NA)
    points(p[p>1],v[p>1],pch=21,col="black",bg=cmp[p>1])
    points(rep(xmin+(1-xmin)/2,sum(p==1)),v[p==1],pch=21,col="black",bg=cmp[p==1])
    if (legend == TRUE){
      if (length(use)>1){
        graphics::legend("topright",c(paste0("AL",ychar[1,use[1]]),paste0("AL",ychar[1,use[length(use)]])),
                         col="black",pt.bg=c(cmp[1],cmp[length(use)]),pch=21,cex=1,bg="white")
      } else {
        graphics::legend("topright",paste0("AL",ychar[1,use[1]]),col="black",pt.bg=cmp[1],pch=21,cex=1,bg="white")
      }
    }
  }
  invisible(NULL)
  
}

#' @rdname plot.imapa
#' @export
print.imapa <- function(x,...){
  
  use <- x$summary[6,]==1
  mdl <- x$summary[5,use]
  cat("iMAPA\n")
  cat(paste0("Aggregation levels used: ",sum(use)," (",min(x$summary[1,use]),
             " to ",max(x$summary[1,use]),"), combined with the ",x$fit.info$comb,"\n"))
  cat(paste0("Models selected: Croston ",sum(mdl==1),", SBA ",sum(mdl==2),
             ", SES ",sum(mdl==3),"\n"))
  if (length(x$frc.out)>0){
    cat(paste0("Forecast for h = ",length(x$frc.out),": ",format(signif(x$frc.out[1],4)),
               " per period\n"))
  }
  if (!is.null(x$lt)){
    id.printlt(x$lt)
  }
  invisible(x)
  
}

#' @rdname plot.imapa
#' @export
coef.imapa <- function(object,...){
  
  return(object$model.fit)
  
}

#' @rdname plot.imapa
#' @export
summary.imapa <- function(object,...){
  
  tbl <- data.frame(t(object$summary[1:4,]))
  tbl$model <- c("Croston","SBA","SES")[object$summary[5,]]
  tbl$model[object$summary[6,]==0] <- "Not used"
  tbl <- cbind(tbl,t(object$model.fit[3:6,]))
  rownames(tbl) <- colnames(object$summary)
  out <- list(levels=tbl,comb=object$fit.info$comb,frc.out=object$frc.out,lt=object$lt)
  class(out) <- "summary.imapa"
  return(out)
  
}

#' @rdname plot.imapa
#' @export
print.summary.imapa <- function(x,...){
  
  cat("iMAPA\n\n")
  cat("Aggregation levels:\n")
  print(x$levels,digits=4)
  cat(paste0("\nForecasts combined with the ",x$comb,".\n"))
  if (length(x$frc.out)>0){
    cat(paste0("Forecast for h = ",length(x$frc.out),": ",format(signif(x$frc.out[1],4)),
               " per period\n"))
  }
  if (!is.null(x$lt)){
    id.printlt(x$lt)
  }
  invisible(x)
  
}