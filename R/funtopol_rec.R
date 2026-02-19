#' Species functional role in the general recruitment network
#' @description provides summary information of the frequency of species of
#' each functional role and the classification of species in each role.
#'
#' @inheritParams check_cover
#' @inheritParams check_interactions
#'
#' @returns A list with two elements, one with descriptors of the networks
#' and another with the role of each species.
#'
#' The first element of the list provides the following information:
#' - *Num. nodes*: Total number of nodes in the network.
#' - *Num. edges*: Total number of links in the network.
#' - *Connectance*:  Proportion of links observed relative to all the possible
#' links (*C*). In the case of recruitment networks, we use the
#' formula $C = L /(N^2 - N)$ since the node "Open" does not act as a recruit
#' (i.e. Open is represented by a row of zeroes in the adjacency matrix).
#' - *Num. non-trivial SCCs*: Number of SCCs formed by more than one node
#' in the network.
#' - *Num. core species*: Number of species in the core SCC.
#' - *Prop. core species*: Proportion of species in the core relative to the
#' total number of species in the network.
#' - *Num. satellite species*: Number of satellite species in the network.
#' - *Prop. satellite species*: Proportion of satellite species relative to
#' the total number of species in the network.
#' - *Num. disturbance-dependent transients*: Number of disturbance-dependent
#' transient species in the network.
#' - *Prop. disturbance-dependent transients*: Proportion of
#' disturbance-dependent transient species relative to the total number of
#' species in the network.
#' - *Num. strict transients*: Number of strict transient species in the network.
#' - *Prop. strict transients*: Proportion of strict transient species
#' relative to the total number of species in the network.
#' - *Qualitative Persistence*: The sum of the proportion of core and
#' satellite species.
#' The second element of the list, provides the role for each plant species
#' in the network, whose interpretations differ in each plant-plant interaction
#'  type. Each node (i.e. species) can be:
#'  - Part of the core **core**. In the case of *general recruitment networks*,
#'   the direction of the links represents that the space occupied by canopy
#'   species (predecessor node) will be replaced by the species recruiting
#'   under its canopy (successor node). In this case, all species in the core
#'   must recruit, at least, under the canopy of another core species, and
#'   allow the recruitment of at least another core species.
#'  - A **satellite** species: these are non-core species that can be reached
#'  from some core species, following the direction of the arrows.
#'  In *general recruitment networks*, these are species that recruit under
#'  the canopy of some core species but that do not show recruitment of any
#'  core species under their canopy.
#'  - A **disturbance-dependent transient** species: these are species that
#'  can be reached from the "Open" node but not from core or satellite species
#'  (i.e. for example, pioneer species that only recruit away from
#'  established plants).
#'  - A **strict transient** species: a species that cannot be reached from
#'  any other node. In the case of *general recruitment networks* strict
#'  transients are those species that do not recruit in the studied local
#'  assemblage (e.g., a pioneer species in a mature forest patch or a relict
#'  species at it distribution limit).
#'
#'
#' @export
#'
#' @examples
#' funtopol_rec(Amoladeras_int,Amoladeras_cover)
#'
#'
#'
funtopol_rec <- function(int_data,cover_data){

  int_data<-comm_to_RN(int_data,cover_data,expand="yes", rm_sp_no_cover="allsp" )

  if (!"Open" %in% int_data$Canopy) stop("ERROR: your data does not contain a node named Open or it is spelled differently.")


  int_data <- int_data[which(int_data$Fcr!=0), c("Canopy", "Recruit")]
  g <- igraph::graph_from_data_frame(int_data, directed = TRUE)
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE)

  if (length(which(int_data$Canopy=="Open"))==0) {
    warning("Open is included as a node in the network, even though no recruits (with cover data of that species) are associated with Open in this community")

    g <- igraph::add_vertices(g, 1, name = "Open")
  }

  NEdges <- igraph::gsize(g)
  NNodes <- igraph::gorder(g)

  CDirected <- NEdges/(NNodes*(NNodes - 1))
  SCCs <- igraph::components(g, mode = "strong")

  if(max(SCCs$csize)>1){

    numSCCs <- SCCs$no
    numNTSCCs <- sum(SCCs$csize > 1)
    coreSize <- max(SCCs$csize)
    SCC_memb <- SCCs$membership
    SCC_memb <- as.data.frame(SCC_memb)
    SCC_subgraphs <- igraph::decompose(g, mode = "strong") # Makes a subgraph of each SCC
    IDcore <- match(coreSize, SCCs$csize) # locates the position of the core in the list of SCCs
    MembersCore <- igraph::V(SCC_subgraphs[[IDcore]])$name # List of the species in the core
    IDOpen <- SCC_memb$SCC_memb[match("Open", row.names(SCC_memb))] # Locate the position of the "open" node in the list of SCCs
    outReachFromOpen <- names(igraph::subcomponent(g, "Open", "out")[-1]) # List of nodes reachable from the "open"
    outReachFromCore <- vector("list", coreSize) # List of nodes reachable from core nodes
    for (i in 1:coreSize) {
      outReachFromCore[[i]] <- igraph::subcomponent(g, MembersCore[i], mode = "out")
    }
    a <- unlist(outReachFromCore)
    a <- unique(names(a))
    MembersSatellites <- setdiff(a, MembersCore)
    MembersTransients <- setdiff(igraph::V(g)$name,c(MembersCore,MembersSatellites))
    MembersTransients <- MembersTransients[!MembersTransients == "Open"]
    MembersStrictTransients <- setdiff(MembersTransients, outReachFromOpen)
    MembersDdTransients <- setdiff(MembersTransients, MembersStrictTransients)
    numSat <- length(MembersSatellites)
    numTransAll <- length(MembersTransients)
    numDdTrans <- length(MembersDdTransients)
    numStrictTrans <- length(MembersStrictTransients)
    propCore <- coreSize/(NNodes - 1)
    propSat <- numSat/(NNodes - 1)
    propTrans <- numTransAll/(NNodes - 1)
    propStrTrans <- numStrictTrans/(NNodes - 1)
    propDdTrans <- numDdTrans/(NNodes - 1)
    persistence <- propCore + propSat

    # Function output

    df <- data.frame(
      c(NNodes,
        NEdges,
        CDirected,
        numNTSCCs,
        coreSize,
        propCore,
        numSat,
        propSat,
        numDdTrans,
        propDdTrans,
        numStrictTrans,
        propStrTrans,
        persistence)
    )
    colnames(df) <- c("Value")
    rownames(df) <- c(
      "Num. nodes",
      "Num. edges",
      "Connectance",
      "Num. non-trivial SCCs",
      "Num. core species",
      "Prop. core species",
      "Num. satellite species",
      "Prop. satellite species",
      "Num. disturbance-dependent transients",
      "Prop. disturbance-dependent transients",
      "Num. strict transients",
      "Prop. strict transients",
      "Qualitative Persistence")

    classif <- list(
      MembersSatellites,
      MembersCore,
      MembersStrictTransients,
      MembersDdTransients)
    classif <- stats::setNames(classif,
                               c("Satellites",
                                 "Core",
                                 "Strict_transients",
                                 "Disturbance_dependent_transients")
    )

    df0 <- classif
    df_Sat <- data.frame(df0$Satellites, rep("Satellite", length(df0$Satellites)))
    colnames(df_Sat) <- c("id", "group")
    df_Core <- data.frame(df0$Core, rep("Core", length(df0$Core)))
    colnames(df_Core) <- c("id", "group")
    df_Str <- data.frame(df0$Strict_transients, rep("Strict_transients", length(df0$Strict_transients)))
    colnames(df_Str) <- c("id", "group")
    df_Ddtr <- data.frame(df0$Disturbance_dependent_transients, rep("Disturbance_dependent_transients", length(df0$Disturbance_dependent_transients)))
    colnames(df_Ddtr) <- c("id", "group")
    df0 <- rbind(df_Sat, df_Core, df_Str, df_Ddtr)
    df0 <- df0[order(df0$id),]

    outputs <- list("Descriptors" = df, "Functional_classification" = df0)

  }else{

    warning("This network does not have a Core, and thus the species membership to different roles can not be defined")

    coreSize<-0

    df <- data.frame(
      c(NNodes,
        NEdges,
        CDirected,
        coreSize))


    colnames(df) <- c("Value")
    rownames(df) <- c(
      "Num. nodes",
      "Num. edges",
      "Connectance",
      "Num. core species")

    df0<-list(
      Satellites = character(0),
      Core = character(0),
      Strict_transients = character(0),
      Disturbance_dependent_transients = character(0)
    )

    outputs <-list("Descriptors" = df, "Functional_classification" = df0)

  }

  return(outputs)

}
