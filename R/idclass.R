#' Time series categorisation for intermittent demand
#'
#' @description Categorise intermittent demand time series according to their average
#' inter-demand interval and the squared coefficient of variation of their non-zero
#' demand, indicating whether they should be modelled with Croston's method, SBA, or
#' SES.
#'
#' @param data Time series dataset, where each column is a series. Alternatively, this
#'   can be a single series.
#' @param type Type of categorisation:
#'   \itemize{
#'   \item \code{"SBC"}: Syntetos-Boylan-Croston;
#'   \item \code{"KH"}: Kostenko-Hyndman (exact);
#'   \item \code{"KHa"}: Kostenko-Hyndman (approximate);
#'   \item \code{"PK"}: Petropoulos-Kourentzes (exact);
#'   \item \code{"PKa"}: Petropoulos-Kourentzes (approximate).
#'   }
#'   The exact schemes are computationally expensive, as SBA is optimised for each
#'   series, unless \code{a.in} is provided.
#' @param a.in Vector of SBA interval smoothing parameters, one for each series. This is
#'   used by the exact categorisations \code{"KH"} and \code{"PK"}. If \code{NULL}, the
#'   parameters are optimised internally using MAR as the cost function.
#' @param outplot Plot of the categorisation:
#'   \itemize{
#'   \item \code{"summary"}: simplified plot that reports the number of series in each
#'   class and the cut-off points;
#'   \item \code{"detail"}: scatterplot of the average inter-demand interval (p) and the
#'   squared coefficient of variation of the non-zero demand (CV^2), with the areas of
#'   each class shaded;
#'   \item \code{"none"}: no plot.
#'   }
#'   Both plots are also available afterwards with \code{plot}, see
#'   \code{\link{plot.idclass}}.
#' @param plot.focus Only relevant to \code{outplot == "detail"}. A vector of two
#'   elements, the maximum p and CV^2 to plot, so that the scatterplot can be focused on
#'   the area separating the classes, for example \code{c(1.5,1.5)}. If \code{NULL}, the
#'   maximums are defined from the dataset.
#'
#' @details The Petropoulos-Kourentzes classification extends the Kostenko-Hyndman one
#'   for series without zero demand, p = 1, which are assigned to SES instead of
#'   Croston's method or SBA. Such series are rare at the original frequency, but common
#'   once a series is temporally aggregated, which is why this classification is used by
#'   \code{\link{imapa}}. Series with fewer than two non-zero values cannot be
#'   categorised; a warning is given and they are left out of all classes.
#'
#' @return An object of class \code{idclass}, which is a list containing:
#' \item{idx.croston}{Index of series that are categorised under Croston.}
#' \item{idx.sba}{Index of series that are categorised under SBA.}
#' \item{idx.ses}{Index of series that are categorised under SES. Provided only for
#'   \code{"PK"} and \code{"PKa"} types.}
#' \item{cv2}{Squared coefficient of variation of non-zero demand.}
#' \item{p}{Average inter-demand interval.}
#' \item{summary}{Summary of number of series under each category.}
#' \item{fit.info}{Information used by the methods of the object. Not intended for
#'   direct use.}
#'
#' @references
#' Petropoulos, F., and Kourentzes, N. (2015). Forecast combinations for intermittent
#' demand. \emph{Journal of the Operational Research Society}, 66(6), 914--924.
#' \doi{10.1057/jors.2014.62}
#'
#' Syntetos, A. A., Boylan, J. E., and Croston, J. D. (2005). On the categorization of
#' demand patterns. \emph{Journal of the Operational Research Society}, 56(5), 495--503.
#'
#' Kostenko, A. V., and Hyndman, R. J. (2006). A note on the categorization of demand
#' patterns. \emph{Journal of the Operational Research Society}, 57(10), 1256--1257.
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{plot.idclass}}, \code{\link{imapa}}, \code{\link{data.frc}}.
#'
#' @examples
#' # Create some data, where each column is a time series
#' set.seed(1)
#' dataset <- simID(100, 60, idi=1.15, cv2=0.3)
#' idclass(dataset)
#'
#' # Detailed plot of the SBC classification
#' cls <- idclass(dataset, type="SBC", outplot="detail")
#' cls$summary
#'
#' # Focus the detailed plot
#' idclass(dataset, type="KHa", outplot="detail", plot.focus=c(1.5,1.5))
#'
#' # Single series
#' idclass(ts.data1, outplot="none")$summary
#'
#' @export idclass
idclass <- function(data,type=c("PKa","SBC","KHa","KH","PK"),a.in=NULL,
                    outplot=c("summary","detail","none"),plot.focus=NULL){

  type <- toupper(type[1])
  if (!(type %in% c("PKA","SBC","KHA","KH","PK"))){
    stop("Input type must be one of: PKa, SBC, KHa, KH, PK.")
  }
  outplot <- id.match(outplot,c("summary","detail","none"),"outplot")

  # Initialise
  if (!isa(data,"data.frame") && (is.null(dim(data)) || sum(dim(data)==1)>0)){
    data <- matrix(as.numeric(data),nrow=length(data))
    N <- 1
    p <- NA
    v <- NA
    if (is.null(a.in)){
      a <- NA
    } else {
      a <- a.in[1]
    }
  } else {
    N <- dim(data)[2]
    p <- vector("numeric",N)    # Average interval
    v <- vector("numeric",N)    # Squared CV
    if (is.null(a.in)){
      a <- vector("numeric",N)  # Interval alpha with SBA
    } else {
      a <- a.in
      if (length(a)!=N){
        stop("Length of a.in must be equal to number of series.")
      }
    }
  }

  few <- vector("logical",N)
  for (i in 1:N){
    # Get time series
    temp <- data[,i]
    temp <- temp[!is.na(temp)]
    # Croston decomposition
    ch <- id.char(temp)
    if (is.na(ch[1])){
      few[i] <- TRUE
      p[i] <- v[i] <- NA
      next
    }
    p[i] <- ch[1]
    v[i] <- ch[2]
    if (is.null(a.in)){
      if (type=="KH" | type=="PK"){
        fit <- crost(temp,h=0,type="sba")
        a[i] <- fit$weights[2]
      }
    }
  }
  if (any(few)){
    warning(paste0(sum(few)," series with fewer than two non-zero values cannot be categorised."))
  }

  # Classify between SBA & Croston
  if (type=="KH" | type=="PK"){
    # Exact Kostenko & Hyndman
    use.sba <- v > (4*p*(2-p)-a*(4-a)-p*(p-1)*(4-a)*(2-a))/(p*(4-a)*(2*p-a))
  } else if (type=="SBC"){
    # Syntetos, Boylan & Croston
    use.sba <- (p > 1.32 | v > 0.49)
  } else {
    # Approximate Kostenko & Hyndman
    use.sba <- v > (2-(3/2)*p)
  }
  use.sba[few] <- FALSE
  use.croston <- !use.sba
  use.croston[few] <- FALSE

  # Classify between SES and Croston/SBA
  if (type=="PK" | type=="PKA"){
    use.ses <- p <= 1
    use.ses[few] <- FALSE
    use.sba[use.ses] <- FALSE
    use.croston[use.ses] <- FALSE
    idx.ses <- which(use.ses)
  } else {
    use.ses <- NULL
    idx.ses <- NULL
  }
  idx.croston <- which(use.croston)
  idx.sba <- which(use.sba)

  # Summarise results
  if (is.null(use.ses)){
    summary <- rbind(sum(use.croston),sum(use.sba))
    rownames(summary) <- c("Croston","SBA")
  } else {
    summary <- rbind(sum(use.croston),sum(use.sba),sum(use.ses))
    rownames(summary) <- c("Croston","SBA","SES")
  }
  colnames(summary) <- "Series"

  out <- list(idx.croston=idx.croston,idx.sba=idx.sba,idx.ses=idx.ses,
              cv2=v,p=p,summary=summary)
  out$fit.info <- list(type=type,use.croston=use.croston,use.sba=use.sba,
                           use.ses=use.ses,few=few)
  class(out) <- "idclass"

  # Produce plots
  if (outplot != "none"){
    plot(out,type=outplot,plot.focus=plot.focus)
  }

  return(out)

}

#-------------------------------------------------
#' Plot and print intermittent demand categorisation
#'
#' @description Plot or print the categorisation of intermittent demand series
#' produced by \code{\link{idclass}}.
#'
#' @param x An object of class \code{idclass}.
#' @param type Type of plot: \code{"summary"} for a simplified plot that reports the
#'   number of series in each class and the cut-off points, or \code{"detail"} for a
#'   scatterplot of the series.
#' @param plot.focus Only relevant to \code{type == "detail"}. A vector of two elements,
#'   the maximum p and CV^2 to plot. If \code{NULL}, the maximums are defined from the
#'   dataset.
#' @param ... Additional arguments passed to \code{\link[graphics]{plot}}, for example
#'   \code{main}.
#'
#' @return \code{plot} has no return value, and \code{print} returns \code{x}
#'   invisibly.
#'
#' @author Nikolaos Kourentzes, \email{nikolaos@@kourentzes.com}
#'
#' @seealso \code{\link{idclass}}.
#'
#' @examples
#' set.seed(1)
#' cls <- idclass(simID(50, 60, idi=1.3, cv2=0.5), outplot="none")
#' cls
#' plot(cls)
#' plot(cls, type="detail", main="Simulated series")
#'
#' @export
plot.idclass <- function(x,type=c("summary","detail"),plot.focus=NULL,...){

  type.cls <- x$fit.info$type
  info <- x$fit.info
  type <- id.match(type,c("summary","detail"),"type")
  p <- x$p
  v <- x$cv2
  keep <- !info$few
  use.sba <- info$use.sba
  use.croston <- info$use.croston
  use.ses <- info$use.ses
  if (type.cls=="PKA"){
    type.plot <- "PKa"
  } else if (type.cls=="KHA"){
    type.plot <- "KHa"
  } else {
    type.plot <- type.cls
  }
  N <- length(p)

  if (type == "detail"){
    pk <- p[keep]
    vk <- v[keep]
    # Set space in plot for PK and PKa types
    if (type.cls=="PK" | type.cls=="PKA"){
      if (!is.null(plot.focus)){
        xmin <- 1-(plot.focus[1]-1)*0.1
      } else {
        xmin <- 1-(max(c(max(pk)+diff(range(pk))*0.1),1.5)-1)*0.1
      }
    } else {
      xmin <- 1
    }
    # Set limits depending if plot.focus is given or not
    if (!is.null(plot.focus)){
      xx <- c(xmin,plot.focus[1])
      yy <- c(0,plot.focus[2])
      plot.out <- (p > plot.focus[1] | v > plot.focus[2])
      plot.out[!keep] <- FALSE
    } else {
      xx <- c(xmin,max(c(max(pk)+diff(range(pk))*0.1),1.5))
      yy <- c(0,max(c(max(vk)+diff(range(vk))*0.1),1.5))
      plot.out <- vector("logical",N)
    }
    # Plot SBA series in focus
    args <- list(x=p[use.sba & !plot.out],y=v[use.sba & !plot.out],
                 type="p",pch=20,xaxs="i",yaxs="i",
                 xlim=xx,ylim=yy,cex=0.8,xlab="p",ylab=parse(text="CV^2"),
                 main=paste(type.plot,"classification"))
    args <- modifyList(args,list(...))
    do.call(plot,args)
    # Plot SBA series out-of-focus
    if (sum(plot.out)>1){
      p.temp <- p[plot.out]
      p.temp[p.temp>xx[2]] <- xx[2]
      v.temp <- v[plot.out]
      v.temp[v.temp>yy[2]] <- yy[2]
      points(p.temp,v.temp,pch=21,bg="darkgray",cex=1)
    }
    # Create shaded area for Croston series
    id.classarea(type.cls)
    # Plot Croston series
    points(p[use.croston],v[use.croston],pch=1,col="red",cex=0.6)
    # Create area for ses
    if (type.cls == "PKA" | type.cls == "PK"){
      polygon(c(0,1,1,0),c(0,0,yy[2],yy[2]),col=gray(0.4),border=NA)
      points(rep(xmin+(1-xmin)/2,sum(use.ses & !plot.out)),v[use.ses & !plot.out],
             col="white",pch=20,cex=0.8)
      if (sum(use.ses & plot.out)>1){
        points(xmin+(1-xmin)/2,yy[2],col="grey",pch=21,bg="white")
      }
    }
  } else {
    # Set plot area
    if (type.cls=="PK" | type.cls=="PKA"){
      xx <- c(0.94,1.5)
    } else if (type.cls=="SBC"){
      xx <- c(1,1.64)
    } else {
      xx <- c(1,1.5)
    }
    yy <- c(0,1)
    # Start a plot
    args <- list(x=0,y=0,type="n",xlim=xx,ylim=yy,xaxs="i",yaxs="i",xlab="p",
                 ylab=parse(text="CV^2"),main=paste(type.plot,"classification"),
                 xaxt="n",yaxt="n")
    args <- modifyList(args,list(...))
    do.call(plot,args)
    # Create shaded area for Croston series
    id.classarea(type.cls)
    # Label ticks
    if (type.cls != "SBC"){
      axis(1,at=c(1,4/3),labels=c("1","4/3"))
      axis(2,at=c(0,0.5),labels=c("0","1/2"))
    } else {
      # Special plot for SBC
      axis(1,at=c(1,1.32),labels=c("1","1.32"))
      axis(2,at=c(0,0.49),labels=c("0","0.49"))
      lines(c(1,xx[2]),c(0.49,0.49),lty=3,col="grey")
      lines(c(1.32,1.32),c(0,yy[2]),lty=3,col="grey")
      text(1.16,0.75,"Erratic",col=gray(0.4))
      text(1.16,0.25,"Smooth",col=gray(0.4))
      text(1.48,0.75,"Lumpy",col=gray(0.4))
      text(1.48,0.25,"Intermittent",col=gray(0.4))
    }
    # Report number of series in each area
    text(xx[2],1,paste("SBA: ",sum(use.sba),sep=""),adj=c(1.2,1.2))
    text(1,0,paste("Croston: ",sum(use.croston),sep=""),adj=c(-0.2,-1.2))
    # Shaded area and text for PK and PKa classifications
    if (type.cls == "PKA" | type.cls == "PK"){
      polygon(c(0,1,1,0),c(0,0,yy[2],yy[2]),col=gray(0.6),border=NA)
      text(0.97,0,paste("SES: ",sum(use.ses),sep=""),adj=c(-0.2,0.2),srt=90)
      lines(c(0.94,1),c(0.5,0.5),lty=3)
    }
  }
  invisible(NULL)

}

#' @rdname plot.idclass
#' @export
print.idclass <- function(x,...){

  type.cls <- x$fit.info$type
  type.plot <- switch(type.cls,"PKA"="PKa","KHA"="KHa",type.cls)
  cat(paste0(type.plot," classification of ",length(x$p)," series:\n"))
  print(x$summary)
  invisible(x)

}

#-------------------------------------------------
id.classarea <- function(type){
# Shade the area of the Croston class in classification plots - internal function

  if (type == "SBC"){
    polygon(c(1,1.32,1.32,1),c(0,0,0.49,0.49),col="lightgrey",border=NA)
  } else if (type == "KHA" | type == "PKA"){
    polygon(c(1,4/3,1),c(0,0,0.5),col="lightgrey",border=NA)
  } else if (type == "KH" | type == "PK"){
    p.x <- c(seq(1,4/3,0.02),4/3)
    a.p <- 1
    v.y <- (4*p.x*(2-p.x)-a.p*(4-a.p)-p.x*(p.x-1)*(4-a.p)*(2-a.p))/(p.x*(4-a.p)*(2*p.x-a.p))
    polygon(c(p.x,1),c(v.y,0),col="lightgrey",border=NA)
    a.p <- 0
    v.y2 <- (4*p.x*(2-p.x)-a.p*(4-a.p)-p.x*(p.x-1)*(4-a.p)*(2-a.p))/(p.x*(4-a.p)*(2*p.x-a.p))
    v.y2[v.y2<0] <- 0
    polygon(c(p.x,p.x[18:1]),c(v.y,v.y2[18:1]),col=gray(0.2,0.3),border=NA)
  }

}
