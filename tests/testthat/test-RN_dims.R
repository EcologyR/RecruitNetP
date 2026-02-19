test_that("RN_dims works for recruitment networks in Amoladeras", {

  out <- RN_dims(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "rec"
  )

  # Output structure
  expect_s3_class(out, "data.frame")
  expect_equal(rownames(out),
               c("Num. Nodes", "Num. Links", "Connectance"))
  expect_true(all(out$Value >= 0))
})

#-------------------
test_that("RN_dims computes connectance correctly for rec networks", {

  out <- RN_dims(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "rec"
  )

  # Manual calculation
  df <- comm_to_RN_UNI(Amoladeras_int, Amoladeras_cover)
  n_nodes <- length(unique(c(df$Canopy, df$Recruit)))
  n_links <- sum(df$Pcr)
  connectance_expected <- n_links / (n_nodes^2 - n_nodes)

  expect_equal(out["Connectance", "Value"], connectance_expected)
})

#---------------------
test_that("RN_dims works for facilitation networks in Amoladeras", {

   out <- RN_dims(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "fac"
  )

  expect_s3_class(out, "data.frame")
  expect_equal(rownames(out),
               c("Num. Nurse sp","Num. Facilitated sp", "Num. Links", "Connectance"))
  expect_true(all(out$Value >= 0))
})

#---------------------
test_that("RN_dims works for competition networks in Amoladeras", {


 out <- RN_dims(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "comp"
  )

  expect_s3_class(out, "data.frame")
  expect_equal(rownames(out),
               c("Num. Canopy depressing sp","Num. Recruit depressed sp", "Num. Links", "Connectance"))
  expect_true(all(out$Value >= 0))
})

#-----------------------------
test_that("RN_dims connectance for bipartite networks is correctly calculated", {


  fac_dims <- RN_dims(Amoladeras_int, Amoladeras_cover, int_type = "fac")

  mat <- RN_to_matrix(Amoladeras_int, Amoladeras_cover,
                      int_type = "fac", weight = "Pcr")

  n_links <- sum(mat)
  connectance_expected <- n_links / (nrow(mat) * ncol(mat))

  expect_equal(fac_dims["Connectance", "Value"], connectance_expected)
})
