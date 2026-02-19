
test_that("visu_topol_fac returns graph and SCC list", {

  expect_silent({
    res <- visu_topol_fac(test_data$com,
                          test_data$cov,
                          direction = "out")
  })

  expect_type(res, "list")
  expect_named(res, c("graph", "scc"))
  expect_s3_class(res$graph, "igraph")
  expect_type(res$scc, "list")
})

#------------------------------------

test_that("Graph has correct number of vertices", {

  res <- suppressWarnings(
    visu_topol_fac(test_data$com,
                   test_data$cov,
                   direction = "out")
  )

  expected_species <- unique(c(test_data$com$Canopy,
                               test_data$com$Recruit))

  expect_equal(vcount(res$graph), length(expected_species))
})

#------------------------------------

test_that("SCC groups match those detected by fac_topol", {

  res_visu <- suppressWarnings(
    visu_topol_fac(test_data$com,
                   test_data$cov,
                   direction = "out")
  )

  res_topol <- fac_topol(test_data$com,
                         test_data$cov,
                         direction = "out")

  # Compare number of SCCs
  expect_equal(length(res_visu$scc),
               length(res_topol$loops$nodes))

  # Compare sizes of SCCs
  visu_sizes <- sort(sapply(res_visu$scc, length))
  topol_sizes <- sort(sapply(res_topol$loops$nodes, length))

  expect_equal(visu_sizes, topol_sizes)
})
#------------------------------------

test_that("Detects SCCs with more than one node", {

  res <- suppressWarnings(
    visu_topol_fac(test_data$com,
                   test_data$cov,
                   direction = "out")
  )

  expect_true(length(res$scc) > 0)
  expect_true(all(sapply(res$scc, length) > 1))
})

#------------------------------------

test_that("Custom layout function works correctly", {

  expect_silent(
    visu_topol_fac(test_data$com,
                   test_data$cov,
                   direction = "out",
                   layout_fun = layout_in_circle)
  )
})

#------------------------------------

test_that("Custom vertex and edge parameters do not break plotting", {

  expect_silent(
    visu_topol_fac(test_data$com,
                   test_data$cov,
                   direction = "out",
                   vertex_size = 10,
                   edge_arrow_size = 0.2)
  )
})



#------------------------------------

test_that("Works with real dataset without errors", {

  expect_silent({
    res <- visu_topol_fac(mysite_com,
                          mysite_cov,
                          direction = "out")
  })

  expect_s3_class(res$graph, "igraph")
})
