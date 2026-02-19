# Test 1.- La función devuelve un dataframe válido
test_that("recruitment_niche_test devuelve dataframe válido", {
  res <- recruitment_niche_test(vent.int, vent.cov)
  expect_s3_class(res, "data.frame")
  expect_true(all(c(
    "Recruit",
    "Fr",
    "Av",
    "Fro",
    "Ao",
    "testability",
    "Significance",
    "Test_type",
    "Veg_effect"
  ) %in% names(res)))
})

# Test 2.- Test type y Veg effect contienen categorías válidas
test_that("Test_type y Veg_effect contienen categorías válidas", {
  res <- recruitment_niche_test(vent.int, vent.cov)
  expect_true(all(res$Test_type %in% c("Binomial", "Chi-square")))
  expect_true(all(res$Veg_effect %in% c(
    
    "Facilitated",
    "Depressed",
    "Neutral",
    "Not testable"
  )))
  expect_false(any(is.na(res$Test_type)))
  expect_false(any(is.na(res$Veg_effect)))
})

# Test 3.- coherencia testability y veg_effect
test_that("Coherencia completa entre testability y Veg_effect", {
  res <- recruitment_niche_test(vent.int, vent.cov)
  # Not testable
  idx_nt <- which(res$testability > 0.05)
  if(length(idx_nt) > 0){
    expect_true(all(res$Veg_effect[idx_nt] == "Not testable"))
  }
  # Facilitated
  idx_fac <- which(res$Veg_effect == "Facilitated")
  if(length(idx_fac) > 0){
    expect_true(all((res$Fr[idx_fac]/res$Av[idx_fac]) >
                      (res$Fro[idx_fac]/res$Ao[idx_fac])))
  }
  # Depressed
  idx_dep <- which(res$Veg_effect == "Depressed")
  if(length(idx_dep) > 0){
    expect_true(all((res$Fr[idx_dep]/res$Av[idx_dep]) <
                      (res$Fro[idx_dep]/res$Ao[idx_dep])))
  }
})

# Test 4.- se lanza el warning sobre diferentes tipos de tests  
test_that("lanza warning cuando hay varios tipos de tests", {
  expect_warning(
    recruitment_niche_test(vent.int, vent.cov),
    "Different tests were used")
})

