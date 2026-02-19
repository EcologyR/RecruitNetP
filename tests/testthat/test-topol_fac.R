
test_that("topol_fac provides the correct structure", {
  res <- topol_fac(Amoladeras_int, Amoladeras_cover, direction = "out")

  expect_type(res, "list")
  expect_named(res, c("loops", "simple"))

  # loops
  expect_named(res$loops, c("summary", "nodes"))
  expect_s3_class(res$loops$summary, "data.frame")
  expect_type(res$loops$nodes, "list")

  # simple
  expect_named(res$simple, c("summary", "nodes"))
  expect_s3_class(res$simple$summary, "data.frame")
  expect_type(res$simple$nodes, "list")
})

#------------------------------------
test_that("loops summary coincides with the actual nodes ", {
  res <- topol_fac(Amoladeras_int, Amoladeras_cover, direction = "out")

  expect_equal(nrow(res$loops$summary), 1)

  scc_id <- res$loops$summary$scc_id
  n_nodos_reportado <- res$loops$summary$n_nodos

  nodos_reales <- res$loops$nodes[[as.character(scc_id)]]

  expect_equal(length(nodos_reales), n_nodos_reportado)
})

#------------------------------------

test_that("simple summary coincides with real paths", {
  res <- topol_fac(Amoladeras_int, Amoladeras_cover, direction = "out")

  summary_df <- res$simple$summary
  nodes_list <- res$simple$nodes

  for(i in seq_len(nrow(summary_df))) {

    nodo <- summary_df$nodo[i]
    path_index <- summary_df$path_index[i]
    n_nodes_in_path <- summary_df$n_nodes_in_path[i]

    path_real <- nodes_list[[nodo]][[path_index]]

    expect_equal(length(path_real), n_nodes_in_path)
  }
})

#------------------------------------

test_that("simple paths start by its original node", {
  res <- topol_fac(Amoladeras_int, Amoladeras_cover, direction = "out")

  for(nodo in names(res$simple$nodes)) {
    for(path in res$simple$nodes[[nodo]]) {
      expect_equal(path[1], nodo)
    }
  }
})

#------------------------------------

test_that("simple paths do not have repeted nodes", {
  res <- topol_fac(Amoladeras_int, Amoladeras_cover, direction = "out")

  for(nodo in names(res$simple$nodes)) {
    for(path in res$simple$nodes[[nodo]]) {
      expect_equal(length(path), length(unique(path)))
    }
  }
})

#------------------------------------

test_that("terminal species in a path has a length path of 1", {
  res <- topol_fac(Amoladeras_int, Amoladeras_cover, direction="out")

  terminales <- c("Artemisia_campestris",
                  "Maytenus_senegalensis",
                  "Teucrium_lusitanicum")

  for(sp in terminales) {
    expect_equal(
      res$simple$summary$n_nodes_in_path[
        res$simple$summary$nodo == sp
      ],
      1
    )
  }
})

#------------------------------------

test_that("all nodes in summary are nodes", {
  res <- topol_fac(Amoladeras_int, Amoladeras_cover, direction="out")

  expect_true(all(res$simple$summary$nodo %in% names(res$simple$nodes)))
})
