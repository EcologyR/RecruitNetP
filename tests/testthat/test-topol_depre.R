#################################################
#Tests para topol_depre
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

test_that("topol_depre returns correct list structure (real data)", {
  
  res <- topol_depre(mysite_com, mysite_cov, direction = "out")
  
  expect_type(res, "list")
  expect_named(res, c("loops", "simple"))
  expect_named(res$loops, c("summary", "nodes"))
  expect_named(res$simple, c("summary", "nodes"))
})

#------------------------------------

test_that("Real dataset (Amoladeras) contains no reciprocal loops", {
  
  res <- topol_depre(mysite_com, mysite_cov, direction = "out")
  
  expect_equal(nrow(res$loops$summary), 0)
  expect_length(res$loops$nodes, 0)
})

#------------------------------------

test_that("Dummy dataset detects strongly connected components (loops)", {
  
  res <- topol_depre(test_data$com, test_data$cov, direction = "out")
  
  expect_true(nrow(res$loops$summary) > 0)
  expect_true(length(res$loops$nodes) > 0)
  
  # Each SCC should contain more than one node
  expect_true(all(res$loops$summary$n_nodos > 1))
})

#------------------------------------

test_that("Simple paths (out) have correct structure", {
  
  res <- topol_depre(mysite_com, mysite_cov, direction = "out")
  
  expect_true(nrow(res$simple$summary) > 0)
  expect_true(all(res$simple$summary$n_nodes_in_path >= 1))
  
  for (node in names(res$simple$nodes)) {
    paths <- res$simple$nodes[[node]]
    for (p in paths) {
      expect_equal(p[1], node)
    }
  }
})

#------------------------------------

test_that("Simple paths (in) have correct structure", {
  
  res <- topol_depre(mysite_com, mysite_cov, direction = "in")
  
  expect_type(res, "list")
  expect_true(nrow(res$simple$summary) > 0)
})

#------------------------------------

test_that("Dummy dataset works for both directions", {
  
  res_out <- topol_depre(test_data$com, test_data$cov, direction = "out")
  res_in  <- topol_depre(test_data$com, test_data$cov, direction = "in")
  
  expect_true(nrow(res_out$simple$summary) > 0)
  expect_true(nrow(res_in$simple$summary) > 0)
})

#------------------------------------

test_that("No path is a subpath of another (out direction)", {
  
  res <- topol_depre(mysite_com, mysite_cov, direction = "out")
  
  es_subpath <- function(p, q) {
    lp <- length(p)
    lq <- length(q)
    if (lp >= lq) return(FALSE)
    for (i in 1:(lq - lp + 1)) {
      if (all(q[i:(i + lp - 1)] == p)) return(TRUE)
    }
    FALSE
  }
  
  for (paths in res$simple$nodes) {
    if (length(paths) > 1) {
      for (i in seq_along(paths)) {
        for (j in seq_along(paths)) {
          if (i != j) {
            expect_false(es_subpath(paths[[i]], paths[[j]]))
          }
        }
      }
    }
  }
})

#------------------------------------

test_that("Invalid direction argument throws error", {
  
  expect_error(
    topol_depre(mysite_com, mysite_cov, direction = "sideways")
  )
})
