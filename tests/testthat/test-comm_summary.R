test_that("comm_summary works", {
  Ventisquero <- comm_subset(RecruitNet, site = "Ventisquero")
  out <- comm_summary(Ventisquero)
  expect_equal(out,
               structure(
                 list(
                   `Local Community` = "Ventisquero",
                   Country = "Spain",
                   Latitude = 37.61555556,
                   Longitude = -3.734166667,
                   `Year of sampling` = 2020L,
                   `Site responsible` = "Julio M. Alcántara",
                   Biome = "Mediterranean Forests  Woodlands and Scrub",
                   Vegetation = "Forest",
                   `Plant Community` = "Quercus ilex forest",
                   `Successional stage` = "Late",
                   Disturbance = NA,
                   `Sampling method` = "RN",
                   `Number of plots` = 20L,
                   `Plot area (m2)` = 500L,
                   `Area sampled (m2)` = 10000L,
                   `Number of plant species` = 43L,
                   `Contains Open node` = "Yes",
                   `Number of woody species` = 39L,
                   `Number of herb species` = 4L,
                   `Number of other types` = 0L
                 ),
                 row.names = "Value",
                 class = "data.frame"
               ))
})
