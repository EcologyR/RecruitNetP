

#' Functional topology of a recruitment network.
#'
#' @description
#' Alcantara & Rey (2012) derived a qualitative way to infer potential species
#' persistence from their position in the RNs based on a combination of non-negative
#' matrix theory and graph theory. Basically, directed unipartite graphs can be
#' unambiguously dissected into 'SCCs'. SCCs are the largest possible subgroups of
#' nodes connected so that all the nodes in a subgroup can be reached from all others
#' following the directions of the links. In the case of RNs, we can define five
#' types of SCCs which play different functional roles:
#'
#' - The "core" of the network is the SCC formed by the largest number of species.
#'   All species in the core must recruit, at least, in the vicinity of another core
#'   species, and allow the recruitment of at least another core species.
#' - “Satellites” are non-core species that can be reached from some core species,
#'   following the direction of the arrows. For example, a satellite species is one
#'   that recruits in the vicinity of some core species but that does not show
#'   recruitment of any species in its vicinity.
#' - "Disturbance-dependent transients" are species that can be reached from the open
#'   node but not from core or satellite species (i.e. for example, species that only
#'   recruit away from established plants).
#' - "Strict transients" are species that cannot be reached from any other node (i.e.
#'   those that do not recruit in the studied local assemblage).
#'
#' Assuming that the dynamics of the system is linear and time-invariant (LTI dynamics),
#' like in Markov models (Horn, 1975, Siles et al., 2008), only core and satellite
#' species will persist in equilibrium in the absence of disturbance. When the dynamics
#' are non-linear, this result cannot be guaranteed to hold, but simulations have shown
#' that the probability of persistence is higher and the time to extinction is longer
#' for core and satellite than for transient species (Alcantara et al., 2017). The sum
#' of core and satellite species is a qualitative approximation to the number of species
#' that can potentially persist.
#'
#' @param int_data Data frame with columns named "Canopy" and "Recruit" identifying
#' observed interactions (i.e. interactions with frequency > 0).
#'
#' @return The function returns two outputs: a data frame with the numeric summary of the
#' functional structure and a list containing the lists of species of each functional
#' type (core, satellite, strict transients and disturbance-dependent transients).
#'
#' @export
#'
#' @examples
#' data(RecruitNet)
#' int_data <- comm_subset(RecruitNet, site = "Ventisquero")
#' Ventisquero_funtopol <- funtopol(int_data)
#' Ventisquero_funtopol$Descriptors
#' Ventisquero_funtopol$Functional_classification


funtopol <- function(int_data){

  if (!"Canopy" %in% names(int_data)) stop("ERROR: your interactions data lacks a column named: Canopy")
  if (!"Recruit" %in% names(int_data)) stop("ERROR: your interactions data lacks a column named: Recruit")
  if (!"Open" %in% int_data$Canopy) stop("ERROR: your data does not contain a node named Open or it is spelled differently.")

  int_data <- int_data[c("Canopy", "Recruit")]
  g <- igraph::graph_from_data_frame(int_data, directed = TRUE)
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE)
  NEdges <- igraph::gsize(g)
  NNodes <- igraph::gorder(g)
  CDirected <- NEdges/(NNodes*(NNodes - 1))
  SCCs <- igraph::components(g, mode = "strong")
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

  outputs <- list("Descriptors" = df, "Functional_classification" = classif)

  return(outputs)

}
