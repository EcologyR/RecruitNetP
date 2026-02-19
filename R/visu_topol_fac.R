#' Title
#'
#' @param int_data
#' @param cover_data
#' @param layout_fun
#' @param vertex_size
#' @param edge_arrow_size
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
visu_topol_fac <- function(int_data,
                           cover_data,
                           layout_fun = layout_with_fr,
                           vertex_size = 20,
                           edge_arrow_size = 0.4) {




  # 1. Rebuild graph (same logic as inside depre_topol)
  M <- RN_to_matrix(int_data, cover_data, int_type = "fac", weight = "Pcr")

  species <- union(rownames(M), colnames(M))

  A <- matrix(0,
              nrow = length(species),
              ncol = length(species),
              dimnames = list(species, species))

  A[rownames(M), colnames(M)] <- M
  M <- t(A)

  g <- graph_from_adjacency_matrix(M,
                                   mode = "directed",
                                   diag = FALSE)

  # 2. Detect SCCs
  scc <- igraph::components(g, mode = "strong")
  scc_groups <- split(V(g)$name, scc$membership)

  # Keep only real loops (>1 node)
  scc_groups <- scc_groups[sapply(scc_groups, length) > 1]

  # 3. Assign colors
  vertex_colors <- rep("grey80", vcount(g))
  names(vertex_colors) <- V(g)$name

  if (length(scc_groups) > 0) {

    palette_colors <- grDevices::rainbow(length(scc_groups))

    for (i in seq_along(scc_groups)) {
      vertex_colors[scc_groups[[i]]] <- palette_colors[i]
    }
  }

  # 4. Plot
  plot(g,
       layout = layout_fun(g),
       vertex.color = vertex_colors,
       vertex.size = vertex_size,
       vertex.label.cex = 0.8,
       vertex.frame.color = "black",
       edge.arrow.size = edge_arrow_size,
       main = paste0("Enhancement recruitment loops (Nurse -> Recruit)"))

  # Return invisibly for further manipulation if needed
  invisible(list(graph = g,
                 scc = scc_groups))
}
