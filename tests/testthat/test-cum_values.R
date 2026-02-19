#################################################
#Tests for function: cum_values
#################################################

test_that("cum_values works", {
  Amoladeras <- comm_subset(RecruitNet, site = "Amoladeras")
  cum_ecount <- cum_values(Amoladeras, property="ecount", k=10)
  cum_vcount <- cum_values(Amoladeras, property="vcount", k=10)
  cum_edge_density <- cum_values(Amoladeras, property="edge_density", k=10)
  expect_equal(dim(cum_ecount$Data), c(200,2))
  expect_equal(length(unique(cum_ecount$Data$sampleSize)),20)
  expect_equal(cum_ecount$Data$Value[200], 229)
  expect_equal(cum_vcount$Data$Value[200], 26)
  expect_equal(cum_edge_density$Data$Value[200], 0.3523077)
})