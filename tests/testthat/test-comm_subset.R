

test_that("comm_subset works fine with one site", {

  skip_if_offline() 
  skip_on_cran()
  skip_on_ci()
  RecruitNet<-load_RN()
  out <- comm_subset(RecruitNet, site = "Ventisquero")
  expect_true(unique(out$Study_site == "Ventisquero"))

})


