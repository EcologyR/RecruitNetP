
# Test 1.- la función valida o no argumentos erróneos
test_that("visu_net valida argumentos correctamente", {
  expect_error(visu_net(Amoladeras_int, Amoladeras_cover,
                        int_type = "cherrypickers"))
  expect_error(visu_net(Amoladeras_int, Amoladeras_cover,
                        weight = "machupichu"))
  expect_error(visu_net(Amoladeras_int, Amoladeras_cover,
                        mode = "cucurucho"))
})

# Test 2.- La función lanza el warning rec bi
test_that("visu_net lanza warning para rec + bi", {
  expect_warning(
    visu_net(Amoladeras_int, Amoladeras_cover,
             int_type = "rec",
             mode = "bi"),
    "recruitment networks"
  )
})

# Test 3.- la función de detiene cunado tiene valores weight incompatibles
test_that("visu_net detiene si weight incompatible en rec", {
  expect_error(
    visu_net(Amoladeras_int, Amoladeras_cover,
             int_type = "rec",
             weight = "Ns",
             mode = "uni"),
    "inconsistency"
  )
})

# Test 4.-  La función  devuelve un objeto igraph unipartito
test_that("visu_net devuelve objeto igraph en modo unipartito", {
  skip_if_not_installed("igraph")
  g <- visu_net(Amoladeras_int, Amoladeras_cover,
                int_type = "fac",
                mode = "uni")
  expect_s3_class(g, "igraph")
  # Opcional pero recomendable:
  expect_true(igraph::vcount(g) > 0)
  expect_true(igraph::ecount(g) > 0)
})
# Test 5.- La función no devuelve igraph, no tiene return() ==> espera un NULL
test_that("visu_net modo bi no devuelve igraph", {
  skip_if_not_installed("bipartite")
  res <- visu_net(Amoladeras_int, Amoladeras_cover,
                  int_type = "fac",
                  mode = "bi")
  expect_null(res)
})
