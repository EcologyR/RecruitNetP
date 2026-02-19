#################################################
#Tests for function: accum_curve
#################################################

test_that("accum_curve works", {
  Amoladeras <- Amoladeras_int
  set.seed(123)
  accum_ecount <- accum_curve(Amoladeras, property="ecount", k=10)
  accum_vcount <- accum_curve(Amoladeras, property="vcount", k=10)
  accum_edge_density <- accum_curve(Amoladeras, property="edge_density", k=10)
  expect_equal(dim(accum_ecount$Data), c(200,2))
  expect_equal(length(unique(accum_ecount$Data$sampleSize)),20)
  expect_equal(accum_ecount$Data$Value[200], 229)
  expect_equal(accum_vcount$Data$Value[200], 26)
  expect_equal(accum_edge_density$Data$Value[200], 0.352307692)
})
