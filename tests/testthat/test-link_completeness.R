test_that("link_completeness works", {
  data(RecruitNet)

  out <- link_completeness(RecruitNet, "Ventisquero", "incidence")
  expect_equal(out, structure(
    list(`Incidence based estimate` = c(299, 488.535555555556, 0.612033242206868, 0.8813)),
    class = "data.frame",
    row.names = c("Lobs", "Lest", "Completeness Links (q=0)", "Coverage Links (q=1)")
  ))


  out <- link_completeness(RecruitNet, "Laxe", "abundance")
  expect_equal(out, structure(
    list(`Abundance based estimate` = c(99, 115.660283416316, 0.855955018229141, 0.9923)),
    class = "data.frame",
    row.names = c("Lobs", "Lest", "Completeness of links (q=0)", "Coverage of links (q=1)")
  ))

  expect_warning(link_completeness(RecruitNet, "Laxe", "incidence"))

  expect_error(link_completeness(RecruitNet, "LosReyes", "incidence"))

})
