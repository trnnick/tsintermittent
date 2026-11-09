test_that("all plots run", {
  pdf(NULL)
  on.exit(dev.off())
  fit <- crost(ts.data1,h=3,probs=c(0.9,0.95),outplot=TRUE)
  expect_silent(plot(fit))
  expect_silent(plot(fit,view="leadtime",main="Test",legend=FALSE))
  expect_silent(crost(ts.data1,h=3,outplot="leadtime"))
  expect_silent(tsb(ts.data1,outplot="period"))
  expect_silent(sexsm(ts.data1,h=1,outplot=TRUE))
  expect_silent(crost.ma(ts.data1,h=2,outplot="leadtime"))
  expect_error(plot(crost(ts.data1,h=0),view="leadtime"),"h of at least 1")
  fit <- imapa(ts.data1,h=3,maximumAL=12)
  for (v in list(1,2,3,4,"period","levels","selection","selection.detail","leadtime")){
    expect_silent(plot(fit,view=v))
  }
  expect_silent(imapa(ts.data1,h=3,maximumAL=6,outplot="leadtime"))
  set.seed(1)
  d <- simID(20,60,idi=1.3)
  cls <- idclass(d,outplot="none")
  expect_silent(plot(cls))
  expect_silent(plot(cls,type="detail",plot.focus=c(1.5,1.5)))
  for (tp in c("SBC","KHa","PKa")){
    expect_silent(idclass(d,type=tp,outplot="detail"))
  }
})
