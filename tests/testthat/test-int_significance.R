test_that("int_significance works for recruitment network in Amoladeras", {


  out <- int_significance(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "rec"
  )

  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
  expect_true("Test_type" %in% colnames(out))
})

#----------------------
test_that("int_significance works for facilitation network in Amoladeras", {


  out <- int_significance(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "fac"
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(out$Effect_int == "Enhancing"))
})

#----------------
test_that("int_significance works for competition network in Amoladeras", {

  out <- int_significance(
    int_data   = Amoladeras_int,
    cover_data = Amoladeras_cover,
    int_type   = "comp"
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(out$Effect_int == "Depressing"))
})

#----------------

test_that("int_significance fails in Amoladeras if 'Open' canopy is missing", {


  int_data <- Amoladeras_int
  int_data$Canopy <- as.character(int_data$Canopy)
  int_data <- int_data[int_data$Canopy != "Open", ]

  expect_error(
    int_significance(
      int_data   = int_data,
      cover_data = Amoladeras_cover,
      int_type   = "rec"
    )
  )
})

#--------------------
test_that("int_significance message appears when multiple statistical tests are used (Amoladeras)", {

  expect_message(
    int_significance(
      int_data   =   Amoladeras_int,
      cover_data =   Amoladeras_cover,
      int_type   = "rec"
    ),
    "Different tests were used",
    all = FALSE
  )
})
