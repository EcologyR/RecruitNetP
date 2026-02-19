
test_that(" Core has a green color", {

  net <- visu_funtopol_rec(Amoladeras_int, Amoladeras_cover)

  groups <- net$x$options$groups

  expect_true("Core" %in% names(groups))
  expect_equal(groups$Open$color$background, "#F0E442")
  expect_equal(groups$Core$color$background, "#009E73")
  expect_equal(groups$Satellite$color$background, "#0072B2")
  expect_equal(groups$Strict_transients$color$background, "#D55E00")
  expect_equal(groups$Disturbance_dependent_transients$color$background, "#CC79A7")
})

#------------------------------------

test_that("No nodes wrongly clasified in the Core visualization", {

  ft <- funtopol_rec(Amoladeras_int, Amoladeras_cover)$Functional_classification
  non_core <- ft$id[ft$group != "Core"]

  net <- visu_funtopol_rec(Amoladeras_int, Amoladeras_cover)
  nodes_vis <- net$x$nodes

  wrongly_core <- intersect(
    nodes_vis$id[nodes_vis$group == "Core"],
    non_core
  )

  expect_length(wrongly_core, 0)
})

#------------------------------------

test_that("All nodes appera in the visualization", {

  core_expected <- funtopol_rec(Amoladeras_int, Amoladeras_cover)$Functional_classification |>
    subset(group == "Core") |>
    (\(x) x$id)()

  net <- visu_funtopol_rec(Amoladeras_int, Amoladeras_cover)

  expect_true(all(core_expected %in% net$x$nodes$id))
})

