# Remove the class and the fit.info element of package objects, so that their list
# elements can be compared with outputs of earlier versions.
strip <- function(x){
  if (is.list(x)){
    if (inherits(x,c("tsint","imapa","idclass"))){
      x <- unclass(x)
      x$fit.info <- NULL
    }
    x[] <- lapply(x,strip)
  }
  x
}
