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
#' # RN_dims_UNI() modified to enter the interaction and cover data
#The size of a network can be described by the number of nodes (*N*) and links (*L*), and its complexity is #proportional to network connectance. *C* can be estimated on different ways depending on the type of #network, but it always measures the proportion of links relative to the maximum number of links that could #be possible in the network. In the case of recruitment networks we use the formula $C = L /(N^2-N)$ since #the node "Open" does not act as a recruit (i.e. Open is represented by a row of zeroes in the adjacency #matrix).
#For facilitation and competition that are bipartite networks, the nodes in eahc guild are #provided( nurse/facilitated, Canopy (depressor)/recruit) seprately, and open is not a node
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

    int_data <-suppressWarnings(RN_to_matrix(int_data,cover_data, int_type="fac",weight="Pcr"))

    df <-int_data
    n_nodes_nurse <-dim(df)[2]
    n_nodes_facilitated <-dim(df)[1]
    n_links <- sum(df)
    connectance <-n_links/(n_nodes_nurse*n_nodes_facilitated)

    out <- data.frame(c(n_nodes_nurse,n_nodes_facilitated, n_links, connectance))
    colnames(out) <- c("Value")
    rownames(out) <- c("Num. Nurse sp","Num. Facilitated sp", "Num. Links", "Connectance")
    return(out)

  }


  if(int_type=="comp"){

    int_data <-suppressWarnings(RN_to_matrix(int_data,cover_data, int_type="comp",weight="Pcr"))

    df <-int_data
    n_nodes_canopy_depressing <-dim(df)[2]
    n_nodes_recruit_depressed <-dim(df)[1]
    n_links <- sum(df)
    connectance <-n_links/(n_nodes_canopy_depressing*n_nodes_recruit_depressed)

    out <- data.frame(c(n_nodes_canopy_depressing, n_nodes_recruit_depressed, n_links, connectance))
    colnames(out) <- c("Value")
    rownames(out) <- c("Num. Canopy depressing sp","Num. Recruit depressed sp", "Num. Links", "Connectance")
    return(out)


  }


}

