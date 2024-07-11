#' Function RN_matrixForm
#'
#' For some analyses, the data must be stored as a matrix rather than data frame.
#' The next function takes a RN stored as data frame and transforms it into a matrix.
#' The output matrix can be used input for bipartite package.
#'
#' @param RNdata A data frame generated with [comm_subset()] or a data.frame
#'  containing columns with all the information needed for the basic analysis of
#'   recruitment networks: canopy species (canopy), recruit species (recruit),
#'   cover of the canopy (cj) and recruit (ci),
#'   and columns with possible weighting variables.
#'
#' @param weight The name of the column of the data frame to be used as weight
#' variable for the links.
#'
#' @return A matrix object with the species names as row (recruit species) and
#' column names (canopy species), and cells indicating the chosen weight for
#' each interaction. For example, the output of function local_RN provides the
#' number of recruits of species i interacting with canopy species j (fij),
#' the number of plots (i.e. incidence) where the interaction has been found (Tij),
#' and the binary presence/absence of the interaction in the whole study site (Pij).
#' Can be used as bipartite input.
#'
#' @export
#'
#' @examples
#' data(RecruitNet)
#' data(CanopyCover)
#' Ventisquero_RNc <- comm_to_RN(RecruitNet, CanopyCover, site = "Ventisquero")
#' Ventisquero_matrix <- RN_to_matrix(Ventisquero_RNc, weight = "fij")
#' Ventisquero_matrix <- RN_to_matrix(Ventisquero_RNc, weight = "Tij")

RN_to_matrix <- function(RNdata = NULL, weight = NULL){

  list_Canopy <- sort(unique(RNdata$Canopy))
  list_Recruit <- sort(unique(RNdata$Recruit))
  Num_canopy <- length(list_Canopy)
  Num_recruit <- length(list_Recruit)
  RNmat <- RNdata[[weight]]
  dim(RNmat) <- c(Num_recruit, Num_canopy)
  colnames(RNmat) <- list_Canopy
  rownames(RNmat) <- list_Recruit
  return(RNmat)

}
