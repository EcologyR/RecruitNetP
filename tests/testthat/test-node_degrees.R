#################################################
#Tests for function: node_degrees
#################################################

test_that("node_degrees works", {
  Amoladeras_com <- comm_subset(RecruitNet, site = "Amoladeras")
  Amoladeras_cov <- comm_subset(CanopyCover, site = "Amoladeras")
  node_degree_rec <- node_degrees(Amoladeras_com, Amoladeras_cov, int_type = "rec")
  node_degree_fac <- node_degrees(Amoladeras_com, Amoladeras_cov, int_type = "fac")
  node_degree_comp <- node_degrees(Amoladeras_com, Amoladeras_cov, int_type = "comp")
  
  expect_equal(unlist(node_degree_rec[1, 1:8]), c("Artemisia_barrelieri", 1, 5, 18, 3.85179402240326, 7, 90, 3.15278474320294), check.attributes = FALSE)
  expect_equal(dim(node_degree_rec), c(24,8))
  expect_equal(unlist(node_degree_fac$Canopy[1, 1:3]), c("Artemisia_barrelieri", 1, 4), check.attributes = FALSE)
  expect_equal(dim(node_degree_fac$Canopy), c(18,3))
  expect_equal(unlist(node_degree_fac$Recruit[1, 1:3]), c("Artemisia_barrelieri", 90, 3), check.attributes = FALSE)
  expect_equal(dim(node_degree_fac$Recruit), c(19,3))
  expect_equal(unlist(node_degree_comp$Canopy[1, 1:3]), c("Asparagus_albus", 27.725, 2), check.attributes = FALSE)
  expect_equal(dim(node_degree_comp$Canopy), c(13,3))
  expect_equal(unlist(node_degree_comp$Recruit[1, 1:3]), c("Artemisia_barrelieri", 90, 2), check.attributes = FALSE)
  expect_equal(dim(node_degree_comp$Recruit), c(7,3))
  
})