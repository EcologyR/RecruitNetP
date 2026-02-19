#################################################
#Tests for function: RN_heatmap
#################################################

test_that("RN_heatmat returns a ggplot object", {
  Amoladeras_com <- comm_subset(RecruitNet, site = "Amoladeras")
  Amoladeras_cov <- comm_subset(CanopyCover, site = "Amoladeras")
  heatm_rec_Fcr <- RN_heatmap(Amoladeras_com,Amoladeras_cov, int_type="rec", weight ="Fcr")
  expect_is(heatm_rec_Fcr, "ggplot")
  heatm_fac_Ns <- RN_heatmap(Amoladeras_com,Amoladeras_cov, int_type="fac", weight ="Ns")
  expect_is(heatm_fac_Ns, "ggplot")
  heatm_comp_RII <- RN_heatmap(Amoladeras_com,Amoladeras_cov, int_type="comp", weight ="RII")
  expect_is(heatm_comp_RII, "ggplot")
})