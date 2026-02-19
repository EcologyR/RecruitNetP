#################################################
#Tests para node_topol
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

test_that("node_topol devuelve NULL si int_type no es válido", {

  res <- node_topol(mysite_com, mysite_cov, int_type = "wrong")
  expect_null(res)
})

#------------------------------------

test_that("node_topol rec devuelve estructura correcta", {

  res <- node_topol(mysite_com, mysite_cov, int_type = "rec")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 24)

  expect_equal(
    colnames(res),
    c("Eigenvector centrality",
      "Extended canopy service",
      "Extended recruitment niche")
  )
})


#------------------------------------
test_that("node_topol fac devuelve estructura correcta", {

  res <- node_topol(mysite_com, mysite_cov, int_type = "fac")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 21)

  expect_equal(
    colnames(res),
    c("Eigenvector nurse centrality",
      "Extended nurse service",
      "Extended facilitated niche")
  )
})

#------------------------------------

test_that("node_topol comp devuelve estructura correcta", {

  res <- node_topol(mysite_com, mysite_cov, int_type = "comp")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 17)

  expect_equal(
    colnames(res),
    c("Eigenvector canopy centrality",
      "Extended canopy depression effect",
      "Extended recruitment depression ")
  )
})


#------------------------------------

test_that("Valores agregados correctos para Artemisia_barrelieri en rec", {

  res <- node_topol(mysite_com, mysite_cov, int_type = "rec")


  fila <- res[rownames(res) == "Artemisia_barrelieri", ]

  expect_equal(fila$`Eigenvector centrality`, 0.384,tolerance = 1e-6)
  expect_equal(fila$`Extended canopy service`, 20)
  expect_equal(fila$`Extended recruitment niche`, 20)

})

#------------------------------------

test_that("Valores agregados correctos para Artemisia_barrelieri en fac", {

  res <- node_topol(mysite_com, mysite_cov, int_type = "fac")


  fila <- res[rownames(res) == "Artemisia_barrelieri", ]

  expect_equal(fila$Eigenvector centrality, 0.3921,tolerance = 1e-6)
  expect_equal(fila$Extended canopy service, 18)
  expect_equal(fila$Extended recruitment niche, 17)

})

#------------------------------------

test_that("Valores agregados correctos para Artemisia_barrelieri en comp", {

  res <- node_topol(mysite_com, mysite_cov, int_type = "comp")


  fila <- res[rownames(res) == "Artemisia_barrelieri", ]

  expect_equal(fila$Eigenvector centrality, 0,tolerance = 1e-6)
  expect_equal(fila$Extended canopy service, 0)
  expect_equal(fila$Extended recruitment niche, 2)

})






