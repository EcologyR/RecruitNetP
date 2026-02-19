#' Basic network dimensions
#'
#'
#' @description
#' Calculates basic descriptors of the interactions network, such as its size
#' described by the number of nodes and links, and its complexity, which is
#' proportional to network connectance.
#'
#' @inheritParams int_significance
#'
#' @returns
#' A table with the following information:
#' - *Num nodes*: Number of nodes in the network (*N*). In the case of
#' facilitation and competition the nodes are provided for each guild
#' - *Num. links*: Number of links in the network (*L*).
#' - *Connectance*: Proportion of links observed from all the possible
#' links (*C*). In the case of general recruitment networks, we use the
#' formula $C = L / (N^2 - N)$ since the node "Open" does not act as a recruit
#' (i.e. Open is represented by a row of zeroes in the adjacency matrix).
#' For facilitation and recruitment depressing networks, connectance is
#' calculated as $C = L /(N_c N_r)$, where *N_c* and *N_r* are the number
#' of canopy and recruit species, respectively.
#'
#' int_type**: Indicates the type of plant-plant interaction that will be
#' presented in the output (recruitment patterns, recruitment enhancement
#' (i.e. facilitation) or recruitment depression (i.e. competition))
#' - *rec*: Estimates the number of nodes, links and connectance of the network
#' based on all plant-plant interactions that contribute to recruitment. "Open"
#' is considered an additional canopy species category.
#'- *fac*: Estimates the number of nodes, links and connectance of the
#' network based on only those pairwise interactions that significantly
#' enhance  recruitment. Not every interaction detected in the field has
#' to be present in the matrix. Non-detected interactions are not considered
#' and "Open" is not included as a canopy species category.
#' - *comp*: Estimates the number of nodes, links and connectance of the network
#' based on only those pairwise interactions that depress recruitment. Not every
#' interaction detected in the field has to be present in the matrix. However,
#' in this case non-detected interactions are considered. "Open" is not
#' included as a canopy species category.
#'
#'
#' @export
#'
#' @examples
#
#'RN_dims(Amoladeras_int, Amoladeras_cover, int_type="rec")
#'RN_dims(mysite_com, mysite_cov, int_type="fac")
#'RN_dims(mysite_com, mysite_cov, int_type="comp")
#'
#'
#'
#'
RN_dims <- function(int_data,cover_data, int_type=c("rec","fac","comp")){

  if(int_type=="rec"){

    int_data <-comm_to_RN_UNI(int_data,cover_data)

    # FUNCTION
    df <- int_data
    n_nodes <- length(unique(c(df$Canopy, df$Recruit)))
    n_links <- sum(df$Pcr)
    connectance <- n_links/(n_nodes^2 - n_nodes)

    out <- data.frame(c(n_nodes, n_links, connectance))
    colnames(out) <- c("Value")
    rownames(out) <- c("Num. Nodes", "Num. Links", "Connectance")
    return(out)

  }

  if(int_type=="fac"){

    int_data <-suppressWarnings(RN_to_matrix(int_data,cover_data,
                                             int_type="fac",weight="Pcr"))

    df <-int_data
    n_nodes_nurse <-dim(df)[2]
    n_nodes_facilitated <-dim(df)[1]
    n_links <- sum(df)
    connectance <-n_links/(n_nodes_nurse*n_nodes_facilitated)

    out <- data.frame(c(n_nodes_nurse,n_nodes_facilitated, n_links, connectance))
    colnames(out) <- c("Value")
    rownames(out) <- c("Num. Nurse sp","Num. Facilitated sp",
                       "Num. Links", "Connectance")
    return(out)

  }


  if(int_type=="comp"){

    int_data <-suppressWarnings(RN_to_matrix(int_data,cover_data,
                                             int_type="comp",weight="Pcr"))

    df <-int_data
    n_nodes_canopy_depressing <-dim(df)[2]
    n_nodes_recruit_depressed <-dim(df)[1]
    n_links <- sum(df)
    connectance <-n_links/(n_nodes_canopy_depressing*n_nodes_recruit_depressed)

    out <- data.frame(c(n_nodes_canopy_depressing, n_nodes_recruit_depressed,
                        n_links, connectance))
    colnames(out) <- c("Value")
    rownames(out) <- c("Num. Canopy depressing sp","Num. Recruit depressed sp",
                       "Num. Links", "Connectance")
    return(out)


  }


}

