#' calculates eigenvector centrality and the two measures of extended niche.
#'
#' @inheritParams check_interactions
#' @inheritParams check_cover
#' @param int_type defines which interaction type will be calculated
#'
#' @returns data frame
#'
#' a data frame with estimates of the implications of each plant species in indirect interactions with the rest of species, both as canopy and recruit. Specifically, the estimates provided are the following:
#'  For recruitment networks:
#'  - *Eigenvector centrality*: provides the dominant eigenvector of the adjacency matrix of the interactions network. Its entries are interpreted as the centrality of each node. A node is more influential if it is connected to other influential nodes. In the case of recruitment networks, it has been associated with the persistence of species (Alcantara and Rey 2012; Alcantara et al. 2017): species with 0 eigenvector centrality have the lowest probability of persisting in the local community.
#'  - *Extended canopy service*: The extended canopy service of a species is defined as the number of other species whose recruitment it enables, directly or indirectly. It reflects an expansion of the species' out-degree in the interaction network, capturing its broader influence on the recruitment dynamics within the community. Using the logic of replacement dynamics, through a temporal series of replacements, a spatial patch will be dominated successively by plants of different species. The legacies of each species on the soil may remain for long time, so the occupation of a patch by one species can affect the recruitment of many others. From a functional perspective, it can be interpreted in terms of how easily disturbances associated with one species may pervade to the rest of species directly or indirectly; for example, a pest decimating the population of one species would affect many or few other species depending on the size of its extended canopy service.
#'  - *Extended recruitment niche*: The number of species in the community whose presence may allow eventually (directly and indirectly) the recruitment of the focal species. It represents an extension of the species' in-degree, capturing its dependence on the broader network for successful recruitment. From a functional perspective, a species with a large extended recruitment niche will be more susceptible to pests affecting any species of the community, but at the same time it may benefit from the increase in abundance of any species in its neighborhood.
#'   For facilitation networks:
#'   - *Eigenvector centrality*: Provides the dominant eigenvector of the adjacency matrix of the interactions network, and its entries are interpreted as the centralities of the nodes. It provides a relative value (i.e., proportionality) indicating for each nurse plant, the extent to which it tends to facilitate—directly or indirectly—those species that in turn facilitate many others. The centrality of a species is proportional to the sum of the centralities of the species it facilitates.
#'   - *Extended nurse service*: The number of species that a given species facilitates, either directly or indirectly (i.e., the number of nodes that can be reached from a given node).
#'   - *Extended facilitated niche*: The number of nurse species that either directly promote the recruitment of a given species or indirectly facilitate the recruitment of its own nurse plants (i.e. the number of nodes that can reach this node). A high value indicates that this species recruits under nurse plants that themselves recruit under other nurse plants. This metric can reflect how sensitive a species is to the loss of nurse plants, especially when the ratio between the number of nurse species (i.e., degree) and the extended recruitment niche is low.
#'    For competition networks:
#'    - *Eigenvector centrality*: Provides a relative value (i.e., proportionality) indicating for each canopy species, the extent to which it tends to depress the recruitment of those species that, in turn, depresses many others when they act as canopy. The centrality of a species is proportional to the sum of the centralities of the species it negatively affects.
#'    - *Extended competitor effect*: The number of species whose recruitment is depressed by a given canopy species, either directly or indirectly depressing its depressors (i.e., the number of nodes that can be reached from a given node). The odd or even number of links in the chain can lead to different outcomes, with even-numbered chains potentially canceling out some effects, while odd-numbered chains can amplify them.
#'    - *Extended competitors niche*: A high value indicates that the recruitment of this species is depressed by species whose recruitment is depressed by other recruitment-depressing canopy species. It represents the number of recruitment-depressing canopy species that either directly depress its recruitment, or indirectly affect the recruitment of its depressor canopy species (i.e., the number of nodes that can reach this node). The odd or even number of links in the chain can lead to different outcomes, with even-numbered chains potentially canceling out some effects, while odd-numbered chains can amplify them. This metric can reflect the extent to which a species can escape its depressors due to the presence of other species that suppress the recruitment of its depressors, especially when the ratio between the number of depressing canopy species (i.e., degree) and the extended depressed niche is low.
#'    All arguments (options)**:
#'    - **int_type** = c("rec","fac","comp")
#'    Argument 1.
#'    - **int_type**: Indicates the type of plant-plant interaction that will be presented in the output: general recruitment, recruitment enhancement (i.e. facilitation) or recruitment depression (i.e. competition).
#'    Explanation of its options:
#'    - *rec*: Estimates eigenvector centrality and extended neighborhoods for the general recruitment network.
#'    - *fac*: Estimates eigenvector centrality and extended neighborhoods for the facilitation network.
#'    - *comp*:Estimates eigenvector centrality and extended neighborhoods for the competition network.
#'
#' @export
#'
#' @examples
#'
#'summary_node_topol_rec <- node_topol(Amoladeras_int,Amoladeras_cover, int_type="rec")
#'head(summary_node_topol_rec)
#'
#'summary_node_topol_fac <- node_topol(Amoladeras_int,Amoladeras_cover, int_type="fac")
#'head(summary_node_topol_fac)
#'
#'summary_node_topol_comp <- node_topol(Amoladeras_int,Amoladeras_cover, int_type="comp")
#'head(summary_node_topol_comp)
#'
#'
node_topol <- function(int_data,cover_data, int_type=c("rec","fac","comp")){

  if(int_type=="rec"){

    int_data<-comm_to_RN_UNI(int_data,cover_data)

    RN_igraph <- igraph::graph_from_adjacency_matrix(t(RN_to_matrix_UNI(int_data, weight = "Pcr")), mode = "directed")
    eigen_cent <- igraph::eigen_centrality(RN_igraph, directed=TRUE, scale=FALSE, options = list(which="LR"))$vector
    out_neigh <- igraph::neighborhood_size(RN_igraph, order=igraph::gorder(RN_igraph), mode="out", mindist=1)
    in_neigh <- igraph::neighborhood_size(RN_igraph, order=igraph::gorder(RN_igraph), mode="in", mindist=1)
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
    out_neigh <- igraph::neighborhood_size(RN_igraph, order=igraph::gorder(RN_igraph), mode="out", mindist=1)
    in_neigh <- igraph::neighborhood_size(RN_igraph, order=igraph::gorder(RN_igraph), mode="in", mindist=1)
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
    out_neigh <- igraph::neighborhood_size(RN_igraph, order=igraph::gorder(RN_igraph), mode="out", mindist=1)
    in_neigh <- igraph::neighborhood_size(RN_igraph, order=igraph::gorder(RN_igraph), mode="in", mindist=1)
    df <- data.frame(eigen_cent, out_neigh,in_neigh)
    df[, 1] <- round(df[, 1], digits = 4)
    colnames(df) <- c("Eigenvector canopy centrality", "Extended canopy depression effect", "Extended recruitment depression ")
    return(df)



  }
}

