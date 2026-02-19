#################################################
#Tests for function: RN_heatmap
#################################################

test_that("RN_heatmat returns a ggplot object", {
  heatm_rec_Fcr <- RN_heatmap(Amoladeras_int, Amoladeras_cover, int_type="rec", weight ="Fcr")
  expect_s3_class(heatm_rec_Fcr, "ggplot")
  heatm_fac_Ns <- RN_heatmap(Amoladeras_int, Amoladeras_cover, int_type="fac", weight ="Ns")
  expect_s3_class(heatm_fac_Ns, "ggplot")
  heatm_comp_RII <- RN_heatmap(Amoladeras_int, Amoladeras_cover, int_type="comp", weight ="RII")
  expect_s3_class(heatm_comp_RII, "ggplot")
})
