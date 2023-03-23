test_that("the input has the right format", {
  popo<-data.frame(A=4, Canopy_Freq=3)
  expect_error (intertest(db_inter=popo, iteration=100, threshold=5)) #esto debe de dar error porque no tiene el nombre de a svariables que yo quiero


})
