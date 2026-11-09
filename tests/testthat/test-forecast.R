test_that("forecast gives the point forecasts for any horizon", {
  fit <- crost(ts.data1,type="sba")
  f <- forecast(fit,h=4)
  expect_equal(as.numeric(f),rep(fit$frc.out[1],4))
  expect_s3_class(f,"ts")
  expect_equal(tsp(f)[1],tsp(ts.data1)[2]+1/12)
  expect_length(forecast(fit),10)
  # Models fitted with h=0 still forecast from the end of the sample
  fit0 <- crost(ts.data1,type="sba",h=0)
  expect_equal(as.numeric(forecast(fit0,h=2)),rep(fit$frc.out[1],2))
  expect_error(forecast(fit0),"at least 1")
  # Plain vectors give plain forecasts
  expect_false(is.ts(forecast(tsb(as.numeric(ts.data1)),h=2)))
})

test_that("forecast matches frc.out for all methods", {
  for (fit in list(crost(ts.data1),tsb(ts.data1),sexsm(ts.data1),crost.ma(ts.data1),
                   imapa(ts.data1,maximumAL=12))){
    expect_equal(as.numeric(forecast(fit)),fit$frc.out)
  }
})

test_that("a fitted model can be applied to new data without optimisation", {
  y.old <- window(ts.data1,end=c(1985,12))
  for (fun in list(crost,tsb,sexsm)){
    fit <- fun(y.old)
    new <- fun(ts.data1,model=fit)
    expect_equal(coef(new),coef(fit))
    expect_equal(new$initial,fit$initial)
    expect_false(any(new$fit.info$opt))
    expect_equal(as.numeric(forecast(fit,h=3,newdata=ts.data1)),new$frc.out[1:3])
    # Optimise only the initial values
    new <- fun(ts.data2,model=fit,init.opt=TRUE)
    expect_equal(coef(new),coef(fit))
    expect_equal(unname(new$fit.info$opt),c(FALSE,TRUE))
  }
  fit <- crost(y.old,type="sbj",cost="msr")
  new <- crost(ts.data1,model=fit,type="sba",cost="mae")
  expect_equal(new$model,"sbj")
  expect_equal(new$fit.info$cost,"msr")
  fit <- crost.ma(y.old,type="sba")
  new <- crost.ma(ts.data1,model=fit)
  expect_equal(new$order,fit$order)
  expect_equal(new$model,"ma.sba")
  fit <- imapa(y.old,maximumAL=12,comb="median")
  new <- imapa(ts.data1,model.fit=fit)
  expect_equal(new$fit.info$comb,"median")
  expect_equal(as.numeric(forecast(fit,h=3,newdata=ts.data1)),new$frc.out[1:3])
  expect_error(crost(ts.data1,model=tsb(ts.data1)),"output of crost")
})

test_that("lt.dist recalculates the quantiles of a fitted model", {
  fit <- crost(ts.data1,type="sba")
  for (h in c(1,3,6)){
    expect_equal(lt.dist(fit,h=h,probs=c(0.8,0.95)),
                 crost(ts.data1,type="sba",h=h,probs=c(0.8,0.95))$lt)
  }
  fit <- imapa(ts.data1,maximumAL=12)
  expect_equal(lt.dist(fit,h=3,probs=0.9),
               imapa(ts.data1,h=3,maximumAL=12,probs=0.9)$lt)
  expect_error(lt.dist(ts.data1,h=3),"needed")
})
