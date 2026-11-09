#-------------------------------------------------
id.prep <- function(data,na.rm=FALSE){
  # Prepare a single time series for modelling - internal function
  #
  # Inputs:
  #   data        Time series, vector, matrix, or data frame. For data frames only
  #               the first column is used.
  #   na.rm       If TRUE, NA values are removed.
  #
  # Outputs:
  #   y           Numeric vector of the series.
  #   tsp         Time attributes of the series if it was a ts object and no values
  #               were removed, NULL otherwise. Used only for plotting.
  #
  # Nikolaos Kourentzes, 2026 <nikolaos@kourentzes.com>
  
  if (isa(data,"data.frame")){
    if (ncol(data)>1){
      warning("Data frame with more than one columns. Using only first one.")
    }
    data <- data[[1]]
  }
  if (is.ts(data)){
    tsp.in <- tsp(data)
  } else {
    tsp.in <- NULL
  }
  y <- as.numeric(data)
  if (na.rm == TRUE){
    if (any(is.na(y))){
      tsp.in <- NULL
    }
    y <- y[!is.na(y)]
  }
  if (any(y<0,na.rm=TRUE)){
    warning("The time series contains negative values.",call.=FALSE)
  }
  
  return(list(y=y,tsp=tsp.in))
  
}

#-------------------------------------------------
id.match <- function(x,choices,name,alias=NULL){
  # Check that a string option is one of the permitted values - internal function
  #
  # Inputs:
  #   x           Option provided by the user. Only the first element is used.
  #   choices     Permitted values (lower case).
  #   name        Name of the option, used in the error message.
  #   alias       Optional named vector of aliases, e.g. c(crost="croston").
  #
  # Output:
  #   x           The option in lower case.
  
  x <- tolower(x[1])
  if (!is.null(alias) && x %in% names(alias)){
    x <- as.character(alias[x])
  }
  if (is.na(x) || !(x %in% choices)){
    stop(paste0("Input ",name," must be one of: ",paste(choices,collapse=", "),"."))
  }
  return(x)
  
}

#-------------------------------------------------
id.outplot <- function(outplot,views=c("period","leadtime")){
  # Interpret the outplot input of the forecasting functions - internal function
  #
  # Inputs:
  #   outplot     TRUE/FALSE (or 1/0), or the name of a plot view.
  #   views       Permitted view names. The first one is used for TRUE.
  #
  # Output:
  #   NULL if no plot is requested, otherwise the name of the view.
  
  outplot <- outplot[1]
  if (is.logical(outplot) || is.numeric(outplot)){
    if (is.na(outplot) || !(outplot %in% c(0,1))){
      stop(paste0("Input outplot must be TRUE, FALSE, or one of: ",paste(views,collapse=", "),"."))
    }
    if (outplot == TRUE){
      return(views[1])
    } else {
      return(NULL)
    }
  }
  return(id.match(outplot,views,"outplot"))
  
}

#-------------------------------------------------
id.h <- function(h){
  # Check the forecast horizon - internal function
  
  h <- h[1]
  if (!is.numeric(h) || is.na(h) || h<0 || h != round(h)){
    stop("Input h must be a non-negative integer.")
  }
  return(h)
  
}

#-------------------------------------------------
id.probs <- function(probs,h){
  # Check the probabilities for the lead time quantiles - internal function
  #
  # Output:
  #   NULL if no quantiles are to be calculated, otherwise the sorted probabilities.
  
  if (is.null(probs)){
    return(NULL)
  }
  if (!is.numeric(probs) || any(is.na(probs)) || any(probs<=0) || any(probs>=1)){
    stop("Input probs must contain probabilities strictly between 0 and 1.")
  }
  if (h<1){
    warning("Quantiles over the lead time need h of at least 1. Input probs is ignored.",call.=FALSE)
    return(NULL)
  }
  return(sort(unique(probs)))
  
}

#-------------------------------------------------
id.cost <- function(data,frc.in,cost,h=0){
  # Cost functions for optimising all methods - internal function
  #
  # Inputs:
  #   data        Time series (numeric vector).
  #   frc.in      In-sample forecasts, where frc.in[t] is the forecast for period t
  #               made at the end of period t-1. Periods without a forecast are NA.
  #   cost        Cost function:
  #                 "mar"  - Mean absolute rate
  #                 "msr"  - Mean squared rate
  #                 "mae"  - Mean absolute error
  #                 "mse"  - Mean squared error
  #                 "msce" - Mean squared cumulative error over h periods
  #                 "mace" - Mean absolute cumulative error over h periods
  #   h           Horizon for the cumulative errors. Used only by "msce" and "mace".
  #
  # Output:
  #   E           Value of the cost function.
  #
  # Notes:
  # MAR and MSR are described in:
  # N. Kourentzes, 2014, On intermittent demand model optimisation and selection,
  # International Journal of Production Economics, 156: 180-190.
  # The rate is compared with the cumulative mean of the series, which is winsorised
  # over the first 30% of the sample, where it is based on few observations.
  
  if (cost == "mse"){
    E <- data - frc.in
    E <- E[!is.na(E)]
    E <- mean(E^2)
  } else if (cost == "mae"){
    E <- data - frc.in
    E <- E[!is.na(E)]
    E <- mean(abs(E))
  } else if (cost == "mar" || cost == "msr"){
    n <- length(data)
    temp <- cumsum(data)/(1:n)
    n <- ceiling(0.3*n)
    temp[1:n] <- temp[n]
    if (cost == "mar"){
      E <- abs(frc.in - temp)
    } else {
      E <- (frc.in - temp)^2
    }
    E <- E[!is.na(E)]
    E <- sum(E)
  } else {
    # Cumulative errors over h periods
    E <- id.lterrors(data,frc.in,h)
    if (length(E) == 0){
      return(9*10^99)
    }
    if (cost == "msce"){
      E <- mean(E^2)
    } else {
      E <- mean(abs(E))
    }
  }
  
  return(E)
  
}

#-------------------------------------------------
id.lterrors <- function(data,frc.in,h,origin=FALSE){
  # Cumulative forecast errors over h periods from fixed origins - internal function
  #
  # Inputs:
  #   data        Time series (numeric vector).
  #   frc.in      In-sample forecasts, where frc.in[t] is the forecast made at the end
  #               of period t-1. For iMAPA these are forecasts from rolling origins.
  #   h           Number of periods to cumulate over (the lead time).
  #   origin      If TRUE, the origins of the errors are returned as well.
  #
  # Output:
  #   E           Cumulative errors sum(y[t:(t+h-1)]) - sum(forecasts made at t-1), for
  #               all origins where both are available.
  #   If origin is TRUE, a list of E and the origin of each error, i.e., the period at
  #   the end of which the forecast was made (t-1). Origin 0 is the start of the sample.
  #
  # Notes:
  # All methods currently in the package produce a constant forecast over the horizon,
  # so the cumulative forecast made at origin t-1 for periods t,...,t+h-1 is simply
  # h*frc.in[t]. If a method with non-constant multiple steps ahead forecasts (e.g. with
  # trend or seasonality) is added, this must be replaced by the sum of the 1 to h steps
  # ahead forecasts from each origin.
  
  n <- length(data)
  if (h<1 || n<h){
    E <- numeric(0)
    o <- integer(0)
  } else {
    idx <- 1:(n-h+1)
    ya <- as.numeric(stats::filter(data,rep(1,h),sides=1))[idx+h-1]
    E <- ya - h*frc.in[idx]
    o <- idx[!is.na(E)] - 1L
    E <- E[!is.na(E)]
  }
  
  if (origin == TRUE){
    return(list(E=E,origin=o))
  }
  return(E)
  
}

#-------------------------------------------------
id.ltdist <- function(E,frc,h,probs,qtype=7,origin=NULL){
  # Empirical quantiles of the demand over the lead time - internal function
  #
  # Inputs:
  #   E           In-sample cumulative errors over h periods.
  #   frc         Out-of-sample forecast (demand rate) from the end of the sample.
  #   h           Lead time.
  #   probs       Probabilities of the quantiles.
  #   qtype       Type of quantile calculation, see stats::quantile.
  #   origin      Origin of each cumulative error, see id.lterrors.
  #
  # Output:
  #   List with the lead time, probabilities, cumulative forecast, quantiles, safety
  #   stock, cumulative errors, their origins, number of errors, and quantile type.
  
  frc <- h*frc[1]
  nE <- length(E)
  pnames <- paste0(format(100*probs,trim=TRUE),"%")
  if (nE == 0){
    warning("No cumulative errors are available over the lead time. Quantiles cannot be calculated.",call.=FALSE)
    q <- rep(NA,length(probs))
  } else {
    q <- as.numeric(quantile(E,probs,type=qtype,names=FALSE))
    if (nE < 1/(1-max(probs))){
      warning(paste0("Only ",nE," cumulative errors are available over the lead time. ",
                     "The empirical quantile at probability ",max(probs)," is unreliable."),call.=FALSE)
    }
  }
  # Demand cannot be negative
  Q <- pmax(frc + q,0)
  ss <- Q - frc
  names(Q) <- names(ss) <- pnames
  
  return(list(h=h,probs=probs,frc=frc,quantile=Q,ss=ss,errors=E,origin=origin,n=nE,qtype=qtype))
  
}

#-------------------------------------------------
id.optim <- function(fn,p0,lbound,ubound,brent=FALSE){
  # Optimiser used by all methods - internal function
  #
  # Inputs:
  #   fn          Cost function of the parameter vector.
  #   p0          Starting values.
  #   lbound      Lower bounds.
  #   ubound      Upper bounds.
  #   brent       If TRUE use Brent (single parameter), else Nelder-Mead. With Nelder-Mead
  #               the bounds must be enforced by fn through id.outbounds.
  
  if (brent == TRUE){
    popt <- optim(par=p0,fn,method="Brent",lower=lbound,upper=ubound,
                  control=list(maxit=2000))$par
  } else {
    popt <- optim(par=p0,fn,method="Nelder-Mead",control=list(maxit=2000))$par
  }
  
  return(popt)
  
}

#-------------------------------------------------
id.outbounds <- function(p,lbound,ubound){
  # TRUE if any parameter is outside its bounds - internal function
  
  return(any(!(p>=lbound) | !(p<=ubound)))
  
}

#-------------------------------------------------
id.aggr <- function(y,fout){
  # Non-overlapping temporal aggregation using sums - internal function
  #
  # Inputs:
  #   y           Time series (numeric vector).
  #   fout        Aggregation levels. Levels larger than length(y) are ignored.
  #
  # Outputs:
  #   out         List of aggregated series, named ALx, where x is the aggregation level.
  #   idx         List of indices of the first period of each bucket in y.
  #
  # Notes:
  # The first observations are dropped where needed, so that all buckets are complete
  # and the last bucket ends with the last observation.
  
  n <- length(y)
  fout <- fout[fout>0 & fout<=n]
  if (length(fout) == 0){
    stop("Aggregation levels must be positive integers, smaller than the length of the series.")
  }
  k <- length(fout)
  out <- idx <- vector("list",k)
  for (i in 1:k){
    slost <- n %% fout[i]
    idx[[i]] <- seq((slost+1),(n-fout[i]+1),fout[i])
    out[[i]] <- colSums(matrix(y[(slost+1):n],nrow=fout[i]))
  }
  names(out) <- names(idx) <- paste0("AL",fout)
  
  return(list(out=out,idx=idx))
  
}

#-------------------------------------------------
id.cluster <- function(paral,cl=NULL){
  # Prepare cluster for parallel processing - internal function
  #
  # Inputs:
  #   paral       1 = use an existing cluster; 2 = initialise a new cluster.
  #   cl          An existing cluster, created with parallel::makeCluster.
  #
  # Output:
  #   cl          Cluster to use.
  #   created     TRUE if the cluster was initialised here and must be stopped.
  
  if (paral == 2){
    crs <- parallel::detectCores()
    cl <- parallel::makeCluster(getOption("cl.cores",crs))
    writeLines(paste("Running with",length(cl),"cores"))
    created <- TRUE
  } else {
    # For compatibility with earlier versions look for a cluster named cl,
    # if one is not provided.
    if (is.null(cl)){
      cl <- get0("cl",envir=globalenv())
    }
    if (!inherits(cl,"cluster")){
      stop("paral=1 requires an initialised cluster. Provide it using the input cl, or use paral=2.")
    }
    created <- FALSE
  }
  # Make the package available on the workers. The workers load the installed package,
  # so they must have the same version as this session. Code loaded with
  # devtools::load_all() is not available to them: they either fail to find the package
  # or silently use an older installed version.
  ver <- as.character(utils::packageVersion("tsintermittent"))
  wver <- unlist(parallel::clusterCall(cl,function(){
    if (requireNamespace("tsintermittent",quietly=TRUE)){
      as.character(utils::packageVersion("tsintermittent"))
    } else {
      NA_character_
    }
  }))
  if (any(is.na(wver)) || any(wver != ver)){
    if (created == TRUE){
      parallel::stopCluster(cl)
    }
    if (any(is.na(wver))){
      found <- "cannot load tsintermittent"
    } else {
      found <- paste0("use tsintermittent ",paste(unique(wver),collapse=", "))
    }
    stop(paste0("Parallel processing needs the same version of tsintermittent installed ",
                "on the workers. This session uses version ",ver,", but the workers ",found,
                ". This happens when the package is loaded with devtools::load_all(), as the ",
                "workers cannot see the loaded code. Install the package, e.g., with ",
                "devtools::install(), restart R, and load it with library(tsintermittent)."),
         call.=FALSE)
  }
  
  return(list(cl=cl,created=created))
  
}

#-------------------------------------------------
id.char <- function(y){
  # Average inter-demand interval and squared coefficient of variation - internal function
  
  nzd <- which(y != 0)
  k <- length(nzd)
  if (k<2){
    return(c(p=NA,cv2=NA))
  }
  z <- y[nzd]
  x <- c(nzd[1],nzd[2:k]-nzd[1:(k-1)])
  
  return(c(p=mean(x),cv2=(sd(z)/mean(z))^2))
  
}

#-------------------------------------------------
id.model <- function(model,method){
  # Check that a model to be reused was produced by the right function - internal function
  
  if (!inherits(model,"tsint") || is.null(model$fit.info) || model$fit.info$method != method){
    stop(paste0("Input model must be the output of ",method,"()."))
  }
  return(model)
  
}