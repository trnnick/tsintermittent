test_that("lead time quantiles match a direct calculation", {
  fit <- crost(ts.data1,h=3,type="sba",probs=c(0.9,0.95))
  y <- as.numeric(ts.data1)
  n <- length(y)
  E <- sapply(1:(n-2),function(t){sum(y[t:(t+2)]) - 3*fit$frc.in[t]})
  E <- E[!is.na(E)]
  q <- quantile(E,c(0.9,0.95),names=FALSE)
  expect_equal(unname(fit$lt$quantile),pmax(3*fit$frc.out[1]+q,0))
  expect_equal(unname(fit$lt$ss),unname(fit$lt$quantile)-fit$lt$frc)
  expect_equal(fit$lt$frc,3*fit$frc.out[1])
  expect_equal(fit$lt$n,length(E))
  expect_equal(names(fit$lt$quantile),c("90%","95%"))
})

test_that("lt.dist reproduces the quantiles of the methods", {
  for (fit in list(crost(ts.data1,h=4,probs=0.9),tsb(ts.data1,h=4,probs=0.9),
                   sexsm(ts.data1,h=4,probs=0.9),crost.ma(ts.data1,h=4,probs=0.9))){
    lt <- lt.dist(ts.data1,fit$frc.in,fit$frc.out,h=4,probs=0.9)
    expect_equal(lt,fit$lt)
  }
})

test_that("no quantiles unless probs is given", {
  expect_null(crost(ts.data1,h=3)$lt)
  expect_warning(fit <- crost(ts.data1,h=0,probs=0.9),"h of at least 1")
  expect_null(fit$lt)
})

test_that("probabilities below the median are allowed", {
  fit <- crost(ts.data1,h=3,probs=c(0.2,0.9))
  expect_length(fit$lt$quantile,2)
  expect_true(fit$lt$ss[1] <= 0)
  expect_error(crost(ts.data1,h=3,probs=1.2),"between 0 and 1")
})

test_that("short samples give a warning", {
  expect_warning(tsb(ts.data2,h=6,probs=0.95),"unreliable")
})

test_that("cumulative cost functions need h and change with h", {
  expect_error(crost(ts.data1,h=0,cost="msce"),"h of at least 1")
  w3 <- crost(ts.data1,h=3,cost="mace")$weights
  w12 <- crost(ts.data1,h=12,cost="mace")$weights
  expect_false(isTRUE(all.equal(w3,w12)))
  expect_s3_class(tsb(ts.data1,h=3,cost="msce"),"tsint")
  expect_s3_class(sexsm(ts.data1,h=3,cost="mace"),"tsint")
  expect_s3_class(crost.ma(ts.data1,h=3,cost="msce"),"tsint")
})

test_that("the origins of the cumulative errors are kept", {
  y <- as.numeric(ts.data1)
  for (fit in list(crost(ts.data1,h=3,probs=0.9),sexsm(ts.data1,h=3,probs=0.9),
                   tsb(ts.data1,h=3,probs=0.9))){
    o <- fit$lt$origin
    expect_length(o,fit$lt$n)
    E <- sapply(o,function(oi){sum(y[(oi+1):(oi+3)]) - 3*fit$frc.in[oi+1]})
    expect_equal(fit$lt$errors,E)
  }
  # Croston has no forecast before the first demand, SES forecasts from the start
  expect_equal(min(crost(ts.data1,h=3,probs=0.9)$lt$origin),which(y!=0)[1])
  expect_equal(min(sexsm(ts.data1,h=3,probs=0.9)$lt$origin),0)
  expect_equal(max(crost(ts.data1,h=3,probs=0.9)$lt$origin),length(y)-3)
})
