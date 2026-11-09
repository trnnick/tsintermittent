test_that("rolling origin forecasts reproduce the forecast at the last origin", {
  fit <- imapa(ts.data1,h=3,maximumAL=12,probs=0.9)
  y <- as.numeric(ts.data1)
  fo <- tsintermittent:::imapa.origin.loop(length(y),y,fit$model.fit,"mean")
  expect_equal(fo,fit$frc.out[1])
  fit <- imapa(ts.data1,h=3,maximumAL=12,comb="median",probs=0.9)
  fo <- tsintermittent:::imapa.origin.loop(length(y),y,fit$model.fit,"median")
  expect_equal(fo,fit$frc.out[1])
})

test_that("internal aggregation matches the original MAPA::tsaggr behaviour", {
  y <- 1:23
  ag <- tsintermittent:::id.aggr(y,c(1,5,30))
  expect_equal(names(ag$out),c("AL1","AL5"))
  expect_equal(ag$out$AL5,c(sum(4:8),sum(9:13),sum(14:18),sum(19:23)))
  expect_equal(ag$idx$AL5,c(4,9,14,19))
})

test_that("imapa handles h=0 and a single aggregation level", {
  expect_length(imapa(ts.data1,h=0)$frc.out,0)
  fit <- imapa(ts.data1,maximumAL=1)
  expect_null(dim(fit$frc.in))
  expect_length(fit$frc.in,length(ts.data1))
})

test_that("imapa outplot options are checked", {
  expect_error(imapa(ts.data1,outplot=7),"outplot")
  expect_error(imapa(ts.data1,outplot="other"),"outplot")
})

test_that("the default maximumAL is the maximum of the maximum interval and the frequency", {
  # Monthly series with maximum interval 18
  fit <- imapa(ts.data1)
  expect_equal(max(fit$summary[1,]),max(c(crost.decomp(ts.data1)$interval,12)))
  expect_equal(max(fit$summary[1,]),18)
  # Monthly series with maximum interval 6
  fit <- imapa(ts.data2)
  expect_equal(max(fit$summary[1,]),12)
  # Without a ts object only the maximum interval is used
  fit <- imapa(as.numeric(ts.data2))
  expect_equal(max(fit$summary[1,]),max(crost.decomp(ts.data2)$interval))
  # The frequency is kept when NA values are removed
  y <- ts.data2
  y[3] <- NA
  fit <- imapa(y,na.rm=TRUE)
  expect_equal(max(fit$summary[1,]),12)
  # If both are lower than minimumAL, minimumAL is used
  fit <- imapa(ts.data1,minimumAL=20)
  expect_equal(colnames(fit$summary),"AL20")
})
