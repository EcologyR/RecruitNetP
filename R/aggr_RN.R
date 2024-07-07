#' Prepare recruitment data
#'
#' @description
#'
#' This function makes a data frame with the basic data needed to build the
#' recruitment network: the identity of canopy and recruit species, and several
#' possible weighting variables for the interactions: fij is the number of
#' recruits of species i interacting with canopy species j summed across plots,
#' Tij is the number of plots (i.e. incidence) where the interaction has been found,
#' and Pij is the presence/absence of the interaction in the whole study site.
#'
#' The output contains all possible pairs of interacting species,
#' including those that were not observed, which are assigned weight = 0.
#'
#' @param raw_RN A data frame containing, at least, the columns:
#' "Canopy", "Recruit" and "Frequency".
#'
#' @return A data frame containing the data to build a recruitment network
#' of a local community: "canopy", "recruit", "fij", "Tij" and "Pij".
#'
#' @examples
#' #Ventisquero_RN <- aggr_RN(Ventisquero_raw)

aggr_RN <- function(raw_RN) {

  # Sum the number of recruits per interaction across plots
  RN <- stats::aggregate(Frequency ~ Canopy*Recruit, data = raw_RN, FUN = sum)
  colnames(RN) <- c("Canopy", "Recruit", "fij")
  RN$Tij <- stats::aggregate(Frequency ~ Canopy*Recruit, data=raw_RN, FUN = NROW)[[3]]
  RN$Pij <- ifelse(RN$Tij==0,0,1)
  RN$Canopy <- gsub("[[:space:]]", "_", RN$Canopy)
  RN$Recruit <- gsub("[[:space:]]", "_", RN$Recruit)

  # Incorporate the unobserved interactions
  species_list <- unique(c(RN$Canopy, RN$Recruit))
  df <- expand.grid(Canopy = species_list, Recruit = species_list)
  RN <- merge(df, RN, all = TRUE)
  RN[is.na(RN)] <- 0
  RN$Canopy <- as.character(RN$Canopy)
  RN$Recruit <- as.character(RN$Recruit)
  RN <- RN[which(RN$Recruit!="Open"),] # Remove Open from the Recruit species
  return(RN)

}
