
test_that("RN_to_matrix works (Amoladeras)", {

  Amoladeras_com <- comm_subset(RecruitNet, site = "Amoladeras")
  Amoladeras_cov <- comm_subset(CanopyCover, site = "Amoladeras")

  net <- RN_to_matrix(
    int_data   = Amoladeras_com,
    cover_data = Amoladeras_cov,
    int_type   = "rec",
    weight     = "Fcr"
  )

  expected <- structure(
    c(3L, 0L, 0L, 0L, 0L, 3L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
      0L, 9L, 0L, 0L, 0L, 0L, 2L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 1L, 1L, 1L,
      0L, 4L, 0L, 0L, 0L, 0L, 0L, 2L, 0L, 3L, 0L, 0L, 0L, 0L, 0L, 1L,
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 4L, 0L, 0L, 0L, 0L, 0L, 0L,
      1L, 0L, 0L, 0L, 0L, 0L, 2L, 0L, 4L, 0L, 0L, 0L, 0L, 0L, 0L, 2L,
      0L, 14L, 0L, 5L, 0L, 0L, 1L, 3L, 0L, 0L, 0L, 0L, 0L, 0L, 2L,
      3L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 15L, 0L, 4L, 0L, 7L, 0L, 0L,
      12L, 14L, 4L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L,
      0L, 0L, 1L, 0L, 8L, 0L, 0L, 0L, 5L, 2L, 2L, 1L, 1L, 0L, 0L, 0L,
      5L, 7L, 5L, 35L, 4L, 9L, 0L, 6L, 1L, 1L, 113L, 0L, 168L, 2L,
      8L, 0L, 4L, 14L, 20L, 14L, 36L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 1L, 2L, 20L, 17L, 0L, 3L, 0L, 4L, 0L, 0L, 9L, 0L,
      42L, 0L, 12L, 0L, 0L, 7L, 4L, 0L, 0L, 0L, 23L, 3L, 4L, 7L, 0L,
      4L, 0L, 6L, 0L, 2L, 10L, 1L, 10L, 0L, 41L, 0L, 17L, 0L, 6L, 17L,
      13L, 12L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L,
      0L, 3L, 0L, 7L, 0L, 0L, 0L, 0L, 0L, 0L, 2L, 1L, 0L, 1L, 0L, 0L,
      0L, 0L, 2L, 2L, 19L, 0L, 2L, 0L, 0L, 36L, 0L, 3L, 0L, 3L, 0L,
      0L, 4L, 1L, 0L, 0L, 0L, 54L, 1L, 7L, 18L, 11L, 796L, 34L, 89L,
      6L, 20L, 80L, 0L, 1839L, 0L, 528L, 19L, 60L, 1L, 16L, 308L, 170L,
      1030L, 5L, 4L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 3L, 0L, 1L, 0L, 0L,
      5L, 0L, 3L, 0L, 5L, 0L, 0L, 8L, 9L, 4L, 0L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 5L, 0L, 1L, 0L, 0L, 0L, 0L, 4L, 0L, 0L, 0L, 0L, 4L,
      0L, 0L, 0L, 0L, 2L, 0L, 0L, 2L, 4L, 32L, 1L, 9L, 0L, 5L, 1L,
      0L, 6L, 0L, 46L, 0L, 0L, 0L, 0L, 17L, 5L, 14L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 6L, 3L, 0L, 0L, 0L, 0L, 0L, 8L, 0L, 7L, 2L, 3L, 0L,
      0L, 0L, 12L, 3L, 0L, 0L, 5L, 0L, 0L, 5L, 5L, 58L, 2L, 7L, 1L,
      1L, 2L, 0L, 43L, 0L, 138L, 0L, 11L, 0L, 5L, 54L, 19L, 34L, 1L,
      0L, 2L, 2L, 3L, 2L, 1L, 86L, 3L, 4L, 1L, 2L, 6L, 0L, 7L, 0L,
      133L, 4L, 27L, 0L, 0L, 87L, 65L, 9L, 0L, 0L, 0L, 0L, 0L, 4L,
      0L, 0L, 0L, 8L, 0L, 3L, 0L, 1L, 4L, 0L, 10L, 0L, 0L, 0L, 0L,
      3L, 2L, 5L, 1L, 0L, 0L, 0L, 4L, 0L, 32L, 12L, 4L, 4L, 0L, 13L,
      2L, 0L, 9L, 0L, 40L, 0L, 14L, 0L, 7L, 9L, 3L, 9L, 2L, 0L),
    dim = c(24L, 24L),
    dimnames = list(
      c("Artemisia_barrelieri","Artemisia_campestris","Asparagus_albus",
        "Asparagus_horridus","Ballota_hirsuta","Helichrysum_stoechas",
        "Hyparrhenia_hirta","Launaea_arborescens","Launaea_lanifera",
        "Lycium_intrincatum","Lygeum_spartum","Maytenus_senegalensis",
        "Ononis_natrix","Open","Phagnalon_saxatile","Piptatherum_miliaceum",
        "Salsola_oppositifolia","Stipa_tenacissima","Teucrium_lusitanicum",
        "Teucrium_polium","Thymelaea_hirsuta","Thymus_hyemalis",
        "Whitania_frutescens","Ziziphus_lotus"),
      c("Artemisia_barrelieri","Artemisia_campestris","Asparagus_albus",
        "Asparagus_horridus","Ballota_hirsuta","Helichrysum_stoechas",
        "Hyparrhenia_hirta","Launaea_arborescens","Launaea_lanifera",
        "Lycium_intrincatum","Lygeum_spartum","Maytenus_senegalensis",
        "Ononis_natrix","Open","Phagnalon_saxatile","Piptatherum_miliaceum",
        "Salsola_oppositifolia","Stipa_tenacissima","Teucrium_lusitanicum",
        "Teucrium_polium","Thymelaea_hirsuta","Thymus_hyemalis",
        "Whitania_frutescens","Ziziphus_lotus")
    )
  )

  expect_equal(net, expected)
})

#-------------------------------
test_that("RN_to_matrix rec has non-negative values for Fcr (Amoladeras)", {

  #data(RecruitNet)
  #data(CanopyCover)
  Amoladeras_com <- comm_subset(RecruitNet, site = "Amoladeras")
  Amoladeras_cov <- comm_subset(CanopyCover, site = "Amoladeras")

  net <- RN_to_matrix(
    int_data   = Amoladeras_com,
    cover_data = Amoladeras_cov,
    int_type   = "rec",
    weight     = "Fcr"
  )

  expect_true(all(net >= 0, na.rm = TRUE))
})

#-------------------------------

test_that("RN_to_matrix fac produces sparse matrix (Amoladeras)", {
  #data(RecruitNet)
  #data(CanopyCover)
  Amoladeras_com <- comm_subset(RecruitNet, site = "Amoladeras")
  Amoladeras_cov <- comm_subset(CanopyCover, site = "Amoladeras")


  net <- RN_to_matrix(
    int_data   = Amoladeras_com,
    cover_data = Amoladeras_cov,
    int_type   = "fac",
    weight     = "Fcr"
  )

  # facilitation debería generar muchos ceros
  prop_zero <- sum(net == 0) / length(net)

  expect_true(prop_zero > 0.3)
})

#-------------------------------

test_that("RN_to_matrix comp produces values only for depressing interactions", {
  #data(RecruitNet)
  #data(CanopyCover)
  Amoladeras_com <- comm_subset(RecruitNet, site = "Amoladeras")
  Amoladeras_cov <- comm_subset(CanopyCover, site = "Amoladeras")


  net <- RN_to_matrix(
    int_data   = Amoladeras_com,
    cover_data = Amoladeras_cov,
    int_type   = "comp",
    weight     = "Fcr"
  )

  # matriz con señal (no todo cero)
  expect_true(sum(net != 0) > 0)
})


#-------------------------------

test_that("Changing weight changes matrix values (Amoladeras)", {
  #data(RecruitNet)
  #data(CanopyCover)
  Amoladeras_com <- comm_subset(RecruitNet, site = "Amoladeras")
  Amoladeras_cov <- comm_subset(CanopyCover, site = "Amoladeras")



  net_Fcr <- RN_to_matrix(
    int_data   = Amoladeras_com,
    cover_data = Amoladeras_cov,
    int_type   = "rec",
    weight     = "Fcr"
  )

  net_RII <- RN_to_matrix(
    int_data   = Amoladeras_com,
    cover_data = Amoladeras_cov,
    int_type   = "rec",
    weight     = "RII"
  )

  # misma dimensión
  expect_equal(dim(net_Fcr), dim(net_RII))

  # pero valores distintos
  expect_false(identical(net_Fcr, net_RII))
})

#-------------------------------

test_that("RN_to_matrix keeps Open column but it may be zero for some indices", {

  #data(RecruitNet)
  #data(CanopyCover)
  Amoladeras_com <- comm_subset(RecruitNet, site = "Amoladeras")
  Amoladeras_cov <- comm_subset(CanopyCover, site = "Amoladeras")

  net <- RN_to_matrix(
    int_data   = Amoladeras_com,
    cover_data = Amoladeras_cov,
    int_type   = "rec",
    weight     = "Ns"
  )

  expect_true("Open" %in% colnames(net))
})
