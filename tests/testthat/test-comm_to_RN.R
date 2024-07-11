test_that("comm_to_RN produces expected output", {
  data(RecruitNet)
  data(CanopyCover)
  out <- comm_to_RN(RecruitNet, CanopyCover, site = "Ventisquero")

  expect_equal(head(out),
               structure(
                 list(
                   Canopy = c(
                     "Acer_monspessulanum",
                     "Acer_monspessulanum",
                     "Acer_monspessulanum",
                     "Acer_monspessulanum",
                     "Acer_monspessulanum",
                     "Acer_monspessulanum"
                   ),
                   Recruit = c(
                     "Acer_monspessulanum",
                     "Berberis_hispanica",
                     "Cistus_albidus",
                     "Crataegus_monogyna",
                     "Daphne_gnidium",
                     "Helleborus_foetidus"
                   ),
                   fij = c(2L, 0L, 0L, 20L, 1L, 1L),
                   Tij = c(1L, 0L, 0L, 8L, 1L, 1L),
                   Pij = c(1L, 0L, 0L, 1L, 1L, 1L),
                   cj = c(96.39, 96.39, 96.39, 96.39, 96.39, 96.39),
                   ci = c(96.39, 31.39, 10.15, 214.34, 32.14, 12.76)
                 ),
                 row.names = c(NA, 6L),
                 class = "data.frame"
               ))

})
