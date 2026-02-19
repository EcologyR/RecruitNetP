test_that("remove_no_cover falla con argumento inválido", {

  expect_error(
    remove_no_cover(Amoladeras_int, Amoladeras_cover,
                    rm_sp_no_cover = "loquesea"),
    NA
  )

})

test_that("remove_no_cover allsp devuelve dataframe", {

  res <- remove_no_cover(Amoladeras_int, Amoladeras_cover,
                         rm_sp_no_cover = "allsp")

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)

})

test_that("remove_no_cover onlycanopy devuelve dataframe", {

  res <- remove_no_cover(Amoladeras_int, Amoladeras_cover,
                         rm_sp_no_cover = "onlycanopy")

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)

})

test_that("allsp elimina especies sin cover", {

  int_mini <- data.frame(
    Plot = c(1,1,1),
    Canopy = c("Open","A","B"),
    Recruit = c("X","Y","Z"),
    Frequency = c(1,1,1)
  )

  cov_mini <- data.frame(
    Plot = c(1,1),
    Canopy = c("Open","A"),
    Cover = c(50,20)
  )

  res <- remove_no_cover(int_mini, cov_mini,
                         rm_sp_no_cover = "allsp")

  expect_false("B" %in% res$Canopy)
  expect_false("Z" %in% res$Recruit)

})


test_that("onlycanopy elimina solo canopy sin cover", {

  int_mini <- data.frame(
    Plot = c(1,1,1),
    Canopy = c("Open","A","B"),
    Recruit = c("X","Y","Z"),
    Frequency = c(1,1,1)
  )

  cov_mini <- data.frame(
    Plot = c(1,1),
    Canopy = c("Open","A"),
    Cover = c(50,20)
  )

  res <- remove_no_cover(int_mini, cov_mini,
                         rm_sp_no_cover = "onlycanopy")

  expect_false("B" %in% res$Canopy)

})

