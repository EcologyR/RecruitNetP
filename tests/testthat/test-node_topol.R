
test_that("node_topol returns NULL if int_type no  valido", {

  res <- node_topol(Amoladeras_int, Amoladeras_cover, int_type = "wrong")
  expect_null(res)
})

#------------------------------------

test_that("node_topol rec returns the correct structure", {

  res <- node_topol(Amoladeras_int, Amoladeras_cover, int_type = "rec")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 24)

  expect_equal(
    colnames(res),
    c("Eigenvector centrality",
      "Extended canopy service",
      "Extended recruitment niche")
  )
})


#------------------------------------
test_that("node_topol fac returns the correct structure", {

  res <- node_topol(Amoladeras_int, Amoladeras_cover, int_type = "fac")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 21)

  expect_equal(
    colnames(res),
    c("Eigenvector nurse centrality",
      "Extended nurse service",
      "Extended facilitated niche")
  )
})

#------------------------------------

test_that("node_topol comp returns the correct structure", {

  res <- node_topol(Amoladeras_int, Amoladeras_cover, int_type = "comp")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 17)

  expect_equal(
    colnames(res),
    c("Eigenvector canopy centrality",
      "Extended canopy depression effect",
      "Extended recruitment depression ")
  )
})


