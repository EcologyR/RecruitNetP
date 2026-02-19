#################################################
#Tests para canopy_service_test
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

test_that("canopy_service_test devuelve un data.frame con las columnas correctas", {
  
  res <- canopy_service_test(mysite_com, mysite_cov)
  
  expect_s3_class(res, "data.frame")
  
  expect_equal(
    colnames(res),
    c("Canopy", "Fc", "Ac", "Fro", "Ao",
      "testability", "Significance", "Test_type", "Canopy_effect")
  )
  
  expect_equal(nrow(res), 23)
})


#------------------------------------

test_that("Valores agregados correctos para Artemisia_barrelieri", {
  
  res <- canopy_service_test(mysite_com, mysite_cov)
  
  fila <- res[res$Canopy == "Artemisia_barrelieri", ]
  
  expect_equal(fila$Fc, 18)
  expect_equal(fila$Fc, sum(mysite_com[mysite_com$Canopy=="Artemisia_barrelieri" ,"Frequency"]))
  expect_equal(fila$Fro, 5111)
  expect_equal(fila$Fro, sum(mysite_com[mysite_com$Canopy=="Open" ,"Frequency"]))
  expect_equal(fila$Ac, 1,tolerance = 1e-6)
  expect_equal(fila$Ao, 6927,tolerance = 1e-6)


})


#--------------------------------------

test_that("Clasificación Canopy_effect correcta para especies conocidas", {
  
  res <- canopy_service_test(mysite_com, mysite_cov)
  
  expect_equal(
    res$Canopy_effect[res$Canopy == "Artemisia_barrelieri"],
    "Facilitative"
  )
  
  expect_equal(
    res$Canopy_effect[res$Canopy == "Artemisia_campestris"],
    "Neutral"
  )
})

