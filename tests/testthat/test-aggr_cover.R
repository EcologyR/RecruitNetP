test_that("aggr_cover returns expected output", {
  data("RNCover")
  out <- aggr_cover(RNCover, site = "Ventisquero")
  expect_equal(out,
               structure(list(
                 Canopy = c("Acer_monspessulanum", "Amelanchier_ovalis",
                            "Berberis_hispanica", "Bupleurum_gibraltaricum", "Cistus_albidus",
                            "Crataegus_laciniata", "Crataegus_monogyna", "Cytisus_scoparius",
                            "Daphne_gnidium", "Digitalis_obscura", "Dorycnium_pentaphyllum",
                            "Echinospartium_boissieri", "Fraxinus_angustifolia", "Genista_cinerea",
                            "Helleborus_foetidus", "Helychrisum_stoechas", "Hormathophylla_spinosa",
                            "Juniperus_oxycedrus", "Juniperus_phoenicea", "Lavandula_latifolia",
                            "Linum_sp", "Linus_narbonense", "Open", "Phillyrea_latifolia",
                            "Pinus_halepensis", "Pistacia_terebinthus", "Prunus_mahaleb",
                            "Prunus_spinosa", "Quercus_coccifera", "Quercus_faginea", "Quercus_ilex",
                            "Rhamnus_alaternus", "Rhamnus_lycioides", "Rhamnus_myrtifolius",
                            "Rosa_sp1", "Ruscus_aculeatus", "Staehelina_dubia", "Thymus_mastichina",
                            "Thymus_orospedanus", "Thymus_zygiis", "Ulex_parviflorus"),
                 abundance = c(96.39,
                               0.47, 31.39, 4.69, 10.15, 15.66, 214.34, 1.3, 32.14, 0.68, 0.43,
                               1.5, 0.02, 22.17, 12.76, 0.02, 0.841793735, 255.12, 0.16, 20.25,
                               0.25, 0.47, 6069.11, 251.52, 3.87, 339.81, 0.46, 0.2, 41.5, 232.19,
                               2152.53, 1.78, 10.48, 0.71, 129.5, 1.83, 0.1, 17.11, 0.37, 0.9,
                               25.67)),
                 row.names = c(NA, -41L), class = "data.frame"))
})
