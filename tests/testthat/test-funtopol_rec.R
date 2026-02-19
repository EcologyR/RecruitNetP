#################################################
#Tests para funtopol_rec
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

test_that("funtopol_rec devuelve la estructura correcta", {
  
  res <- funtopol_rec(mysite_com, mysite_cov)
  
  expect_type(res, "list")
  expect_named(res, c("Descriptors", "Functional_classification"))
  
  expect_s3_class(res$Descriptors, "data.frame")
  expect_s3_class(res$Functional_classification, "data.frame")
})

#--------------------------------------------------

test_that("funtopol_rec devuelve los valores esperados en Descriptors", {
  
  res <- funtopol_rec(mysite_com, mysite_cov)
  df  <- res$Descriptors
  
  expect_equal(df["Num. nodes", "Value"], 24)
  expect_equal(df["Num. edges", "Value"], 221)
  expect_equal(df["Connectance", "Value"], 0.4, tolerance = 1e-6)
  expect_equal(df["Num. non-trivial SCCs", "Value"], 1)
  expect_equal(df["Num. core species", "Value"], 19)
})

#--------------------------------------------------

test_that("Clasificación funcional tiene el número correcto de especies", {
  
  res <- funtopol_rec(mysite_com, mysite_cov)
  fc  <- res$Functional_classification
  
  a<-sort(a$Functional_classification$id)
  b<-sort(intersect(unique(mysite_com$Recruit),unique(mysite_cov$Canopy)))
  expect_equal(nrow(fc), 23)
  expect_true(all(c("id", "group") %in% colnames(fc)))
  expect_equal(a, b)

})

##--------------------------------------------------

test_that("Artemisia_barrelieri está clasificada como Core", {
  
  res <- funtopol_rec(mysite_com, mysite_cov)
  fc  <- res$Functional_classification
  
  fila <- fc[fc$id == "Artemisia_campestris", ]
  
  expect_equal(fila$group, "Satellite")
})

# --------------------------------------------------

test_that("Da error si no existe Open", {
  
  com_sin_open <- mysite_com
  com_sin_open$Canopy <- gsub("Open", "OPEN_WRONG", com_sin_open$Canopy)
  
  expect_error(
    funtopol_rec(com_sin_open, mysite_cov),
    "does not contain a node named Open"
  )
})
