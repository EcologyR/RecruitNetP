test_that("multiplication works", {
  expect_equal(multiply(2 , 2), 5)
  expect_error(multiply(2,"a"))

})
