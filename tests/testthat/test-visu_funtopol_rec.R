#################################################
#Tests para visu_funtopol_rec
#################################################

#library(testthat)

#load data
mypath<-getwd()
download_RN() # Run only the first time you use the package.
setwd(mypath)
RecruitNet <-read.csv("RecruitNet.csv")
CanopyCover <-read.csv("CanopyCover.csv")

mysite_com <- comm_subset_UNI(RecruitNet, "Amoladeras")
mysite_cov <- comm_subset_UNI(CanopyCover, "Amoladeras")

#------------------------------------

test_that("Los nodos Core en la visualización coinciden con funtopol_rec()", {
  
  # Output ecológico
  ft <- funtopol_rec(mysite_com, mysite_cov)$Functional_classification
  core_expected <- ft$id[ft$group == "Core"]
  
  # Visualización
  net <- visu_funtopol_rec(mysite_com, mysite_cov)
  nodes_vis <- net$x$nodes
  
  core_visual <- nodes_vis$id[nodes_vis$group == "Core"]
  
  expect_setequal(core_visual, core_expected)
})

#------------------------------------

test_that("Los nodos Core en la visualización coinciden con funtopol_rec()", {
  
  # Output ecológico
  ft <- funtopol_rec(mysite_com, mysite_cov)$Functional_classification
  core_expected <- ft$id[ft$group == "Satellite"]
  
  # Visualización
  net <- visu_funtopol_rec(mysite_com, mysite_cov)
  nodes_vis <- net$x$nodes
  
  core_visual <- nodes_vis$id[nodes_vis$group == "Satellite"]
  
  expect_setequal(core_visual, core_expected)
})

#------------------------------------

test_that("El grupo Core está asignado al color verde correcto", {
  
  net <- visu_funtopol_rec(mysite_com, mysite_cov)
  
  groups <- net$x$options$groups
  
  expect_true("Core" %in% names(groups))
  expect_equal(groups$Open$color$background, "#F0E442")
  expect_equal(groups$Core$color$background, "#009E73")
  expect_equal(groups$Satellite$color$background, "#0072B2")
  expect_equal(groups$Strict_transients$color$background, "#D55E00")
  expect_equal(groups$Disturbance_dependent_transients$color$background, "#CC79A7")
})

#------------------------------------

test_that("No hay nodos mal clasificados como Core en la visualización", {
  
  ft <- funtopol_rec(mysite_com, mysite_cov)$Functional_classification
  non_core <- ft$id[ft$group != "Core"]
  
  net <- visu_funtopol_rec(mysite_com, mysite_cov)
  nodes_vis <- net$x$nodes
  
  wrongly_core <- intersect(
    nodes_vis$id[nodes_vis$group == "Core"],
    non_core
  )
  
  expect_length(wrongly_core, 0)
})

#------------------------------------

test_that("Todos los nodos Core aparecen en la visualización", {
  
  core_expected <- funtopol_rec(mysite_com, mysite_cov)$Functional_classification |>
    subset(group == "Core") |>
    (\(x) x$id)()
  
  net <- visu_funtopol_rec(mysite_com, mysite_cov)
  
  expect_true(all(core_expected %in% net$x$nodes$id))
})

