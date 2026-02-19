
test_that("RN_to_matrix rec has non-negative values for Fcr (Amoladeras)", {

  net <- RN_to_matrix(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "rec",
    weight     = "Fcr"
  )

  expect_true(all(net >= 0, na.rm = TRUE))
})

#-------------------------------

test_that("RN_to_matrix fac produces sparse matrix (Amoladeras)", {


  net <- RN_to_matrix(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "fac",
    weight     = "Fcr"
  )

  # facilitation shpould provide many zeroes
  prop_zero <- sum(net == 0) / length(net)

  expect_true(prop_zero > 0.3)
})

#-------------------------------

test_that("RN_to_matrix comp produces values only for depressing interactions", {


  net <- RN_to_matrix(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "comp",
    weight     = "Fcr"
  )

  # A matrix with signal  (not all zero)
  expect_true(sum(net != 0) > 0)
})


#-------------------------------

test_that("Changing weight changes matrix values (Amoladeras)", {

    net_Fcr <- RN_to_matrix(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "rec",
    weight     = "Fcr"
  )

  net_RII <- RN_to_matrix(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "rec",
    weight     = "RII"
  )

  # same dimension
  expect_equal(dim(net_Fcr), dim(net_RII))

  # but different values
  expect_false(identical(net_Fcr, net_RII))
})

#-------------------------------

test_that("RN_to_matrix keeps Open column but it may be zero for some indices", {

   net <- RN_to_matrix(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "rec",
    weight     = "Ns"
  )

  expect_true("Open" %in% colnames(net))
})
