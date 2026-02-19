#################################################
#Tests for function: comm_to_RN
#################################################

test_that("comm_to_RN works", {
  Amoladeras_com <- comm_subset(RecruitNet, site = "Amoladeras")
  Amoladeras_cov <- comm_subset(CanopyCover, site = "Amoladeras")
  comm_to_RN_rec <- comm_to_RN(Amoladeras_com, Amoladeras_cov, expand="yes",rm_sp_no_cover="allsp")
  comm_to_RN_fac <- comm_to_RN(Amoladeras_com, Amoladeras_cov, expand="no",rm_sp_no_cover="onlycanopy")
  comm_to_RN_comp <- comm_to_RN(Amoladeras_com, Amoladeras_cov, expand="yes",rm_sp_no_cover="onlycanopy")
  
  expect_equal(unlist(comm_to_RN_rec[1, 1:7]), c("Artemisia_barrelieri", "Artemisia_barrelieri", 3, 1, 1, 1, 1), check.attributes = FALSE)
  expect_equal(dim(comm_to_RN_rec), c(576,7))
  expect_equal(unlist(comm_to_RN_fac[1, 1:6]), c("Artemisia_barrelieri", "Artemisia_barrelieri", 3, 1, 1, 1), check.attributes = FALSE)
  expect_equal(dim(comm_to_RN_fac), c(229,6))
  expect_equal(unlist(comm_to_RN_comp[1, 1:6]), c("Artemisia_barrelieri", "Artemisia_barrelieri", 3, 1, 1, 1), check.attributes = FALSE)
  expect_equal(dim(comm_to_RN_comp), c(624,6))
  
  expect_warning(comm_to_RN(Amoladeras_com, Amoladeras_cov, expand="no",rm_sp_no_cover="allsp"))
  
})