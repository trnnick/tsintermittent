test_that("invalid inputs give clear errors", {
  expect_error(tsb(ts.data1,w=0.1),"vector of 2 elements")
  expect_error(crost(ts.data1,init=2),"two values")
  expect_error(crost(ts.data1,type="other"),"type")
  expect_error(crost(ts.data1,cost="other"),"cost")
  expect_error(crost(ts.data1,outplot="other"),"outplot")
  expect_error(crost(c(0,0,1,0)),"two non-zero")
})

test_that("crost accepts the crost alias for Croston's method", {
  expect_equal(crost(ts.data1,type="crost")$model,"croston")
})

test_that("idclass works with a single ts series and few non-zero values", {
  cls <- idclass(ts.data1,outplot="none")
  expect_equal(sum(cls$summary),1)
  expect_warning(cls <- idclass(cbind(c(0,5,0,0,0,0),c(1,0,2,0,3,0)),outplot="none"),"cannot be categorised")
  expect_equal(sum(cls$summary),1)
  expect_true(is.na(cls$p[1]))
})

test_that("data.frc auto forecasts unclassifiable series with SES", {
  set.seed(1)
  d <- simID(4,60,idi=1.5)
  d[,4] <- c(5,rep(0,59))
  expect_warning(fit <- data.frc(d,method="auto",h=3,probs=0.9))
  expect_equal(fit$out[[4]]$model,"sexsm")
  expect_equal(dim(fit$lt.out),c(1,4))
  expect_equal(dim(data.frc(d[,1:3],method="tsb",h=1)$frc.out),c(1,3))
})

test_that("methods return the documented classes", {
  fit <- crost(ts.data1,h=3,probs=0.9)
  expect_s3_class(fit,"tsint")
  expect_s3_class(summary(fit),"summary.tsint")
  expect_named(coef(fit),c("demand","interval"))
  expect_equal(as.numeric(fitted(fit)),fit$frc.in)
  expect_equal(tsp(fitted(fit)),tsp(ts.data1))
  expect_false(is.ts(fitted(crost(as.numeric(ts.data1)))))
  # The time index is dropped when NA values are removed
  y <- ts.data1
  y[5] <- NA
  expect_false(is.ts(fitted(crost(y,na.rm=TRUE))))
  expect_s3_class(fitted(imapa(ts.data1,maximumAL=6)),"ts")
  expect_output(print(fit),"Croston")
  expect_output(print(summary(fit)),"In-sample MAR")
  # No in-sample metric when nothing is optimised
  fit <- crost(ts.data1,w=0.1,init=c(2,5),init.opt=FALSE)
  expect_null(summary(fit)$metric)
  expect_output(print(summary(fit)),"fixed")
  expect_s3_class(imapa(ts.data1,maximumAL=6),"imapa")
})
