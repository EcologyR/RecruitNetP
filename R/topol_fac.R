
#' Title
#'
#' @param int_data
#' @param cover_data
#' @param direction
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
topol_fac <- function(int_data,cover_data, direction=c("in","out")){

  direction <- match.arg(direction)

  M<-RN_to_matrix(int_data, cover_data, int_type="fac",weight="Pcr")

  #make a square matrix to build a unipartite directed graph to assess reciprocal facilitation

  species <- union(rownames(M), colnames(M))

  A <- matrix(0,  nrow = length(species),ncol = length(species),dimnames = list(species, species))
  A[rownames(M), colnames(M)] <- M

  # traspose the matrix to set that species in rows (from) facilitate species in columns (to)

  M<-t(A)

  #generate a graph (A facilitates B)

  g<-graph_from_adjacency_matrix(M, mode="directed", diag=FALSE)

  ###generate a list to save the objects

  indirect_fac <- list(
    loops = list(
      summary = list(),
      nodes   = list()
    ),
    simple = list(
      summary = list(),
      nodes   = list()
    )
  )

  ######
  #Loops of reciprocal facilitation
  #######

  #nodes
  scc <- components(g, mode = "strong")
  scc_groups <- split(V(g)$name, scc$membership)

  scc_groups<-scc_groups[sapply(scc_groups, length) > 1]

  #summary
  table_scc <- data.frame(
    scc_id = seq_along(scc_groups),
    n_nodos = sapply(scc_groups, length)
  )


  ########
  #functions to detect simple paths and filter subpaths
  ########

  #function to detect subpaths

  es_subpath <- function(p, q) {
    lp <- length(p)
    lq <- length(q)

    if (lp >= lq) return(FALSE)

    for (i in 1:(lq - lp + 1)) {
      if (all(q[i:(i + lp - 1)] == p)) {
        return(TRUE)
      }
    }
    FALSE
  }

  #function to filter subpaths

  filter_paths <- function(paths) {

    lens <- sapply(paths, length)
    ord <- order(lens, decreasing = TRUE)
    paths <- paths[ord]

    keep <- rep(TRUE, length(paths))

    for (i in seq_along(paths)) {

      if (!keep[i]) next

      # compare with paths already aceptaded (& longer)
      if (i > 1) {
        for (j in which(keep)[which(keep) < i]) {
          if (es_subpath(paths[[i]], paths[[j]])) {
            keep[i] <- FALSE
            break
          }
        }
      }
    }

    paths[keep]
  }




  #############
  ##Funcions to obtain simple paths (linear)
  #############


  #function to get maximal paths



  #the threshodl indicates the percentage of overlap of two paths to me considered distinct

  maximal_paths_distinct_pruned <- function(g, start, overlap_threshold = 0.75, dir=c("in","out")) {

    best_paths <- list()  # paths filtered by overlap

    #Internal function applying Depth-First Search (DFS) algorithm and prunning
    #How does it work: DFS starts at a root node.
    #It explores an unvisited neighbor and goes “deep” until it can’t go any further.
    #Then it backtracks to explore other neighbors.
    #This process repeats until all reachable nodes have been visited.

    dfs_extend <- function(current_path) {

      last_node <- utils::tail(current_path, 1)

      if(dir=="out"){
        neighbors_exp <- setdiff(neighbors(g, last_node, mode = "out")$name, current_path)

      }

      if(dir=="in"){
        neighbors_exp <- setdiff(neighbors(g, last_node, mode = "in")$name, current_path)

      }


      # Basic case: it can not be extended
      if (length(neighbors_exp) == 0) {

        keep <- TRUE
        remove_idx <- c()

        for (i in seq_along(best_paths)) {
          existing <- best_paths[[i]]

          # calculate oeverlap between founded paths
          n_shared <- length(intersect(current_path, existing))
          overlap <- n_shared / min(length(current_path), length(existing))

          if (overlap >= overlap_threshold) {
            # path too similar to an existing one → discard
            keep <- FALSE
            break
          }

          # remove existing paths that are included in the new one
          if (all(existing %in% current_path)) {
            remove_idx <- c(remove_idx, i)
          }
        }

        if (keep) {
          if (length(remove_idx) > 0) best_paths[remove_idx] <<- NULL
          best_paths[[length(best_paths) + 1]] <<- current_path
        }

        return()
      }

      # early prunning: if the potential extension is going to produce a lot of overlap
      for (n in neighbors_exp) {
        potential_path <- c(current_path, n)

        prune <- FALSE
        for (existing in best_paths) {
          n_shared <- length(intersect(potential_path, existing))
          overlap <- n_shared / min(length(potential_path), length(existing))
          if (overlap >= overlap_threshold) {
            prune <- TRUE
            break
          }
        }

        if (!prune) {
          dfs_extend(potential_path)
        }
      }
    }

    dfs_extend(start)
    return(best_paths)
  }

  if(direction=="out"){

    #############
    ##Obtain simple paths (linear) FROM all nodes
    #############
    #nodes

    # list to save the linea paths for each node

    paths_per_node_out <- vector("list", vcount(g))

    for (i in seq_along(V(g))) {

      nodo <- V(g)[i]

      # subgrafo reachable from this node
      reachable <- subcomponent(g, nodo, mode = "out")
      g_sub <- induced_subgraph(g, reachable)

      # eliminate edges that come back to the initial node
      edges_to_delete <- E(g_sub)[.to(V(g_sub)[name == V(g)$name[i]])]
      g_sub <- delete_edges(g_sub, edges_to_delete)

      # calculate máximum paths
      paths <- maximal_paths_distinct_pruned(g_sub, V(g_sub)$name[V(g_sub)$name == V(g)$name[i]],dir="out")

      # save
      paths_per_node_out[[i]] <- paths
    }

    names(paths_per_node_out) <- V(g)$name


    #summary

    table_paths_out <- lapply(names(paths_per_node_out), function(nodo) {

      paths <- paths_per_node_out[[nodo]]

      # number of paths from this node
      n_paths <- length(paths)

      # number of nodes in each path
      path_lengths <- sapply(paths, length)

      # generate a data.frame per node
      data.frame(
        nodo = nodo,
        path_index = seq_len(n_paths),
        n_nodes_in_path = path_lengths,
        stringsAsFactors = FALSE
      )
    })

    # combine in a single data.frame
    table_paths_out <- do.call(rbind, table_paths_out)

    #save objects

    indirect_fac$loops$summary<-table_scc
    indirect_fac$loops$nodes<-scc_groups
    indirect_fac$simple$summary<-table_paths_out
    indirect_fac$simple$nodes<-paths_per_node_out

  }

  #############
  ##Obtain simple paths (linear) TO all nodes
  #############

  if(direction=="in"){

    #nodes

    paths_per_node_in <- vector("list", vcount(g))

    for (i in seq_along(V(g))) {

      nodo <- V(g)[i]

      # subgrafo reaching this node
      reachable <- subcomponent(g, nodo, mode = "in")
      g_sub <- induced_subgraph(g, reachable)


      # calculate máximum paths distinct between them
      paths <- maximal_paths_distinct_pruned(g_sub, V(g_sub)$name[V(g_sub)$name == V(g)$name[i]], dir="in")

      # save
      paths_per_node_in[[i]] <- paths
    }

    names(paths_per_node_in) <- V(g)$name



    #summary

    table_paths_in <- lapply(names(paths_per_node_in), function(nodo) {

      paths <- paths_per_node_in[[nodo]]

      # number of paths reaching this node
      n_paths <- length(paths)

      # número de nodos en cada path
      path_lengths <- sapply(paths, length)

      # generar un data.frame por nodo
      data.frame(
        nodo = nodo,
        path_index = seq_len(n_paths),
        n_nodes_in_path = path_lengths,
        stringsAsFactors = FALSE
      )
    })

    # combine in a single data.frame
    table_paths_in <- do.call(rbind, table_paths_in)

    #save the objects
    indirect_fac$loops$summary<-table_scc
    indirect_fac$loops$nodes<-scc_groups
    indirect_fac$simple$summary<-table_paths_in
    indirect_fac$simple$nodes<-paths_per_node_in

  }

  return(indirect_fac)
}
