# Test 1: ¿Realmente falla la función si le faltan esas columnas?
test_that("link_completeness exige columnas obligatorias", {
  df <- data.frame(a = 1)
  expect_error(
    link_completeness(df)
  )
})

# Test 2: ¿Realmente la función rechaza valores inválidos (incidencia, abundancia,indicencia, cosas raras)?
test_that("link_completeness valida argumento type", {
  df <- data.frame(
    Plot = 1,
    Canopy = "A",
    Recruit = "B",
    Frequency = 1
  )
  expect_error(
    link_completeness(df, type = "incidencia")
  )
})

# Test 3: ¿Realmente la función falla, en incidence, cuando sólo hay un plot?
test_that("incidence falla con un solo plot", {
  df <- data.frame(
    Plot = 1,
    Canopy = "A",
    Recruit = "B",
    Frequency = 1
  )
  expect_error(
    link_completeness(df, "incidence")
  )
})

# Test 4: ¿Realmente la función falla, en incidencia, cuando hay pocos plots?
test_that("incidence lanza warning con pocos plots", {
  skip_if_not_installed("iNEXT")
  df <- data.frame(
    Plot = c(1,1,1,2,2,3,3,4,5),
    Canopy = c("A","A","B","A","C","B","C","A","C"),
    Recruit = c("X","Y","X","Y","X","X","Y","X","Y"),
    Frequency = 1
  )
  expect_warning(
    link_completeness(df, "incidence")
  )
})
# Test 5: ¿Es correcta la estructura del resultado de la función con incidence?
test_that("incidence devuelve estructura de output correcta", {

  skip_if_not_installed("iNEXT")

  df <- data.frame(
    Plot = c(1,1,1,2,2,3,3,4,5),
    Canopy = c("A","A","B","A","C","B","C","A","C"),
    Recruit = c("X","Y","X","Y","X","X","Y","X","Y"),
    Frequency = 1
  )

  res <- suppressWarnings(link_completeness(df, "incidence"))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_equal(ncol(res), 1)
  expect_true(all(c(
    "Num. Plots sampled",
    "Lobs",
    "Lest",
    "Completeness Links (q=0)",
    "Coverage Links (q=1)"
  ) %in% rownames(res)))

})
# Test 6: ¿Realmente la función lanza el warning en abundance?
test_that("abundance lanza warning conceptual", {
  skip_if_not_installed("iNEXT")
  df <- data.frame(
    Plot = rep(1:3, each = 4),
    Canopy = rep(c("A","B"), 6),
    Recruit = rep(c("X","Y"), 6),
    Frequency = c(1,2,3,1, 2,1,3,2, 1,1,2,2)
  )
  expect_warning(
    link_completeness(df, "abundance"),
    "Abundance-based approach"
  )

})
# Test 7: La función devuelve la estructura correcta del output (nrows, ncols,  colnames)?
test_that("abundance devuelve estructura de output correcta", {

  skip_if_not_installed("iNEXT")

  df <- data.frame(
    Plot = rep(1:3, each = 4),
    Canopy = rep(c("A","B"), 6),
    Recruit = rep(c("X","Y"), 6),
    Frequency = c(1,2,3,1, 2,1,3,2, 1,1,2,2)
  )

  res <- suppressWarnings(link_completeness(df, "abundance"))

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 5)
  expect_equal(ncol(res), 1)

  expect_true(all(c(
    "Num. plants (recruits) sampled",
    "Lobs",
    "Lest",
    "Completeness of links (q=0)",
    "Coverage of links (q=1)"
  ) %in% rownames(res)))

  expect_true(is.numeric(res[,1]))

})


