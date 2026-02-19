#' Title
#'
#' @param int_data
#' @param cover_data
#' @param int_type
#'
#' @returns
#' @export
#'
#' @examples
#'
#' # node_topol_UNI()node_topol(Ventisquero_com,Ventisquero_cov, int_type="rec")
#'
node_topol <- function(int_data,cover_data, int_type=c("rec","fac","comp")){

  if(int_type=="rec"){

    int_data<-comm_to_RN_UNI(int_data,cover_data)

    RN_igraph <- igraph::graph_from_adjacency_matrix(t(RN_to_matrix_UNI(int_data, weight = "Pcr")), mode = "directed")
    eigen_cent <- igraph::eigen_centrality(RN_igraph, directed=TRUE, scale=FALSE, options = list(which="LR"))$vector
    out_neigh <- igraph::neighborhood_size(RN_igraph, order=gorder(RN_igraph), mode="out", mindist=1)
    in_neigh <- igraph::neighborhood_size(RN_igraph, order=gorder(RN_igraph), mode="in", mindist=1)
    df <- data.frame(eigen_cent, out_neigh,in_neigh)
    df[, 1] <- round(df[, 1], digits = 4)
    colnames(df) <- c("Eigenvector centrality", "Extended canopy service", "Extended recruitment niche")
    return(df)

  }

  if(int_type=="fac"){

    my_mat<-RN_to_matrix (int_data,cover_data, int_type="fac", weight="Pcr")

    #complete with 0 to make a square matrix

    all_species <- union(rownames(my_mat), colnames(my_mat))
    new_mat <- matrix(0, nrow = length(all_species), ncol = length(all_species),
                      dimnames = list(all_species, all_species))
    new_mat[rownames(my_mat), colnames(my_mat)] <- my_mat
    my_mat<-new_mat

    RN_igraph <- igraph::graph_from_adjacency_matrix(t(my_mat), mode = "directed")
    RN_igraphN <- igraph::graph_from_adjacency_matrix(my_mat, mode = "directed")

    eigen_cent <- igraph::eigen_centrality(RN_igraphN, directed=TRUE, scale=FALSE, options = list(which="LR"))$vector
    out_neigh <- igraph::neighborhood_size(RN_igraph, order=gorder(RN_igraph), mode="out", mindist=1)
    in_neigh <- igraph::neighborhood_size(RN_igraph, order=gorder(RN_igraph), mode="in", mindist=1)
    df <- data.frame(eigen_cent, out_neigh,in_neigh)
    df[, 1] <- round(df[, 1], digits = 4)
    colnames(df) <- c("Eigenvector nurse centrality", "Extended nurse service", "Extended facilitated niche")
    return(df)


  }

  if(int_type=="comp"){

    my_mat<-RN_to_matrix (int_data,cover_data, int_type="comp", weight="RII")
    my_mat<-ifelse(my_mat<0,1,0)

    #complete with 0 to make a square matrix
    all_species <- union(rownames(my_mat), colnames(my_mat))
    new_mat <- matrix(0, nrow = length(all_species), ncol = length(all_species),
                      dimnames = list(all_species, all_species))
    new_mat[rownames(my_mat), colnames(my_mat)] <- my_mat
    my_mat<-new_mat

    RN_igraph <- igraph::graph_from_adjacency_matrix(t(my_mat), mode = "directed")
    RN_igraphN <- igraph::graph_from_adjacency_matrix(my_mat, mode = "directed")

    eigen_cent <- igraph::eigen_centrality(RN_igraphN, directed=TRUE, scale=FALSE, options = list(which="LR"))$vector
    out_neigh <- igraph::neighborhood_size(RN_igraph, order=gorder(RN_igraph), mode="out", mindist=1)
    in_neigh <- igraph::neighborhood_size(RN_igraph, order=gorder(RN_igraph), mode="in", mindist=1)
    df <- data.frame(eigen_cent, out_neigh,in_neigh)
    df[, 1] <- round(df[, 1], digits = 4)
    colnames(df) <- c("Eigenvector canopy centrality", "Extended canopy depression effect", "Extended recruitment depression ")
    return(df)



  }
}

