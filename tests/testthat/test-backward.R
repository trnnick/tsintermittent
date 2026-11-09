# Outputs of version 1.10 for a grid of calls, stored in baseline.rds, must be
# reproduced exactly, apart from documented bug fixes.
test_that("outputs match version 1.10", {
  b <- readRDS(test_path("baseline.rds"))
  y1 <- ts.data1; y2 <- ts.data2; dd <- b$dd; s1 <- dd[,12]; s2 <- dd[,22]
  # Bug fix: crost.ma combined the orders of tied minima of the grid search
  fixed <- 'data.frc(dd[,1:3],method="crost.ma")'
  for (i in seq_along(b$calls)){
    cl <- b$calls[i]
    old <- b$res[[i]]
    if (inherits(old,"try-error") || cl %in% fixed){
      next
    }
    # The default maximumAL of imapa changed from floor(n/2) to the maximum interval,
    # so calls that rely on it are compared with the earlier default given explicitly
    if (grepl("^imapa\\(",cl) && !grepl("maximumAL|model.fit",cl)){
      arg <- sub("^imapa\\(([^,)]+).*$","\\1",cl)
      cl <- sub("\\)$",paste0(",maximumAL=floor(length(",arg,")/2))"),cl)
    }
    new <- strip(eval(parse(text=cl)))
    if (is.list(old)){
      expect_setequal(names(old),names(new))
      new <- new[names(old)]
    }
    expect_equal(new,old,tolerance=1e-10,info=cl)
  }
})

test_that("crost.ma uses one of the tied minima of the grid search", {
  b <- readRDS(test_path("baseline.rds"))
  y <- b$dd[,2]
  fit <- crost.ma(y)
  k <- length(which(y!=0))-1
  err <- matrix(NA,k,k)
  for (i in 1:k){
    for (j in 1:k){
      frc <- tsintermittent:::crost.ma.fit(y,i,j,"croston")$frc.in
      err[i,j] <- tsintermittent:::id.cost(y,as.numeric(frc),"mar")
    }
  }
  expect_equal(err[fit$order[1],fit$order[2]],min(err))
})

test_that("old access patterns keep working", {
  fit <- crost(ts.data1)
  expect_equal(fit$weights,unclass(fit)$weights)
  set.seed(1)
  d <- simID(3,60,idi=1.5)
  out <- data.frc(d,method="crost",h=4)$out
  expect_equal(dim(sapply(out,get,x="frc.out")),c(4,3))
})

test_that("fit.info is appended without moving or shadowing existing elements", {
  b <- readRDS(test_path("baseline.rds"))
  fit <- crost(ts.data1)
  old <- b$res[["crost(y1)"]]
  expect_equal(names(fit)[seq_along(old)],names(old))
  expect_equal(names(fit)[length(fit)],"fit.info")
  # Partial matching of $ that worked before still works
  expect_equal(fit$w,fit$weights)
  expect_equal(fit$comp,fit$components)
  expect_equal(fit$ini,fit$initial)
  fit <- crost(ts.data1,h=3,probs=0.9)
  expect_equal(names(fit)[(length(fit)-1):length(fit)],c("lt","fit.info"))
  fit <- imapa(ts.data1,maximumAL=6)
  expect_equal(fit$mod,fit$model.fit)
  expect_equal(fit$s,fit$summary)
  fit <- sexsm(ts.data1)
  expect_equal(fit$a,fit$alpha)
})
