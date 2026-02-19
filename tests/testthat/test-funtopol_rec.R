test_that("funtopol_rec returns the correct structure", {

  res <- funtopol_rec(Amoladeras_int, Amoladeras_cover)

  expect_type(res, "list")
  expect_named(res, c("Descriptors", "Functional_classification"))

  expect_s3_class(res$Descriptors, "data.frame")
  expect_s3_class(res$Functional_classification, "data.frame")
})

#--------------------------------------------------

test_that("funtopol_rec returns the values expected by Descriptors", {

  res <- funtopol_rec(Amoladeras_int, Amoladeras_cover)
  df  <- res$Descriptors

  expect_equal(df["Num. nodes", "Value"], 24)
  expect_equal(df["Num. edges", "Value"], 221)
  expect_equal(df["Connectance", "Value"], 0.4, tolerance = 1e-6)
  expect_equal(df["Num. non-trivial SCCs", "Value"], 1)
  expect_equal(df["Num. core species", "Value"], 19)
})

#--------------------------------------------------

test_that("Functional classification has the correct number of species", {

  res <- funtopol_rec(Amoladeras_int, Amoladeras_cover)
  fc  <- res$Functional_classification

  a<-sort(fc$id)
  b<-sort(intersect(unique(Amoladeras_int$Recruit),unique(Amoladeras_cover$Canopy)))
  expect_equal(nrow(fc), 23)
  expect_true(all(c("id", "group") %in% colnames(fc)))
  expect_equal(a, b)

})

##--------------------------------------------------

test_that("Artemisia_campestris is classified as Satellite", {

  res <- funtopol_rec(Amoladeras_int, Amoladeras_cover)
  fc  <- res$Functional_classification

  fila <- fc[fc$id == "Artemisia_campestris", ]

  expect_equal(fila$group, "Satellite")
})

# --------------------------------------------------

test_that("Open non present error", {

  com_sin_open <- Amoladeras_int
  com_sin_open$Canopy <- gsub("Open", "OPEN_WRONG", com_sin_open$Canopy)

  expect_error(
    funtopol_rec(com_sin_open, Amoladeras_cover),
    "does not contain a node named Open"
  )
})


