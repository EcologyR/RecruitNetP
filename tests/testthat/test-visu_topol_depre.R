

test_that("visu_topol_depre returns graph and SCC list", {

  expect_silent({
    res <- visu_topol_depre(test_data$com,
                            test_data$cov)
  })

  expect_type(res, "list")
  expect_named(res, c("graph", "scc"))
  expect_s3_class(res$graph, "igraph")
})

#------------------------------------


test_that("Graph contains correct number of nodes", {

  res <- suppressWarnings(
    visu_topol_depre(test_data$com,
                     test_data$cov))

  expected_species <- unique(c(test_data$com$Canopy,
                               test_data$com$Recruit))

  expect_equal(igraph::vcount(res$graph), length(expected_species[expected_species!="Open"]))
})

#------------------------------------

test_that("SCC detection works when loops exist", {

  res <- suppressWarnings(
    visu_topol_depre(test_data$com,
                     test_data$cov)
  )

  # There should be at least one SCC with >1 node
  expect_true(length(res$scc) > 0)
  expect_true(all(sapply(res$scc, length) > 1))
})

#------------------------------------

test_that("Works with real dataset even if no loops exist", {

  expect_silent({
    res <- visu_topol_depre(test_data$com,
                            test_data$cov)
  })

  expect_type(res$scc, "list")
})


#------------------------------------


test_that("Custom layout function does not break plotting", {

  expect_silent(
    visu_topol_depre(test_data$com,
                     test_data$cov,
                     layout_fun = igraph::layout_in_circle)
  )
})


#------------------------------------

test_that("Custom vertex and edge parameters work", {

  expect_silent(
    visu_topol_depre(test_data$com,
                     test_data$cov,
                     vertex_size = 10,
                     edge_arrow_size = 0.2)
  )
})


