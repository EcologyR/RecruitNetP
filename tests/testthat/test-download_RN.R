test_that("RN is downloaded", {

  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  path = tempdir()
  destfile = "RN.zip"
  download_RN(path, destfile)
  expect_true(file.exists(file.path(path, "RecruitNet.csv")))
  expect_true(file.exists(file.path(path, "CanopyCover.csv")))

})
