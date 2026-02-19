#' Allow the visualization of intransitivity loops of recruitment depression
#'
#' @description with nodes in different SCC identified by different colors (in grey if they
#' do not belong to any SCC).
#' It may be frequent that in recruitment enhancement (i.e. facilitation) or
#' depression (i.e.competition) networks, there is not any SCC as they tend to
#' have smaller dimensions that general recruitment networks.
#'
#' @inheritParams check_interactions
#' @inheritParams check_cover
#' @param layout_fun type of igraph layout (see [igraph::layout_()])
#' @param vertex_size numeric
#' @param edge_arrow_size numeric
#'
#'
#' @returns a plot
#' @export
#'
#' @examples
#' visu_topol_depre(test_data$com,test_data$cov)
#'
visu_topol_depre <- function(int_data,
                             cover_data,
                             layout_fun = igraph::layout_with_fr,
                             vertex_size = 20,
                             edge_arrow_size = 0.4) {


  # 1. Rebuild graph (same logic as inside depre_topol)
  M <- RN_to_matrix(int_data, cover_data, int_type = "comp", weight = "Pcr")

  species <- union(rownames(M), colnames(M))

  A <- matrix(0,
              nrow = length(species),
              ncol = length(species),
              dimnames = list(species, species))

  A[rownames(M), colnames(M)] <- M
  M <- t(A)

  g <- igraph::graph_from_adjacency_matrix(M,
                                   mode = "directed",
                                   diag = FALSE)

  # 2. Detect SCCs
  scc <- igraph::components(g, mode = "strong")
  scc_groups <- split(igraph::V(g)$name, scc$membership)

  # Keep only real loops (>1 node)
  scc_groups <- scc_groups[sapply(scc_groups, length) > 1]

  # 3. Assign colors
  vertex_colors <- rep("grey80", igraph::vcount(g))
  names(vertex_colors) <- igraph::V(g)$name

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
       main = paste0("Depression recruitment loops (Canopy -> Recruit)"))

  # Return invisibly for further manipulation if needed
  invisible(list(graph = g,
                 scc = scc_groups))
}
