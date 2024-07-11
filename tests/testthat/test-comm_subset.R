data(RecruitNet)

test_that("comm_subset works fine with one site", {

  out <- comm_subset(RecruitNet, site = "Ventisquero")
  expect_true(unique(out$Study_site == "Ventisquero"))

})

test_that("comm_subset works fine with >1 site", {

  out <- comm_subset(RecruitNet, site = c("Ventisquero", "Agadir"))
  expect_equal(unique(out$Study_site), c("Agadir", "Ventisquero"))

})
