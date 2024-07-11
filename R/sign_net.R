#' Association significance testing
#'
#' Generate a list with four elements, a data base with the significance tests
#' of association of each interaction (in rows) and three matrices of interaction
#' of positive, negative and neutral interactions, from a study site (for a single sites)
#'
#' @param inter_data data frame with at least four columns:
#' Study_site (unique name of the study site),
#' Recruit (species of the recruit),
#' Canopy (species of the canopy, or "Open"), and
#' Frequency (number of recruits of that species observed under that canopy species
#' in any plot, with "Open" representing recruits observed without a canopy species).
#'
#' @param cover_data data frame with at least five columns:
#' Study_site (unique name of the study site),
#' Plot (unique name of each plot within a study site),
#' Canopy (species of the canopy),
#' Cover (percentage of cover of that species in that plot), and
#' Sampled_distance_or_area (total area of that plot or length in the case of transects).
#'
#' @param site is the name of the Study_site
#'
#' @param type is a character that identifies which hypothesis is being tested with three options:
#' 1) "by_pairwise_interaction": whether the number of recuits of each recruit species under each canopy species is significantly different from
#' the number observed in the open, considering the percentage of cover of that canopy species and open area (i.e.bare ground), respectively.
#' 2) "by_recruit_sp": whether the number of recuits of each recruit species under any canopy species (all together) is significantly different from
#' the number observed in the open, considering the percentage of cover of all canopy species together and open area (i.e.bare ground), respectively.
#' 3) "by_canopy_sp":  whether the number of recuits of any recruit species (all together) under a given canopy species is significantly different from
#' the number observed in the open, considering the percentage of cover of that canopy species and open area (i.e.bare ground), respectively
#' 
#' @return a list with four elements. The first element data frame with the
#' same structure as the input with three additional columns:
#' int_p (p-value of the binomial test of association),
#' int_sign (the sign of the association, being Posive (or Negative) if the
#' association is stronger (or weaker) than expected by the percentage cover
#' of Canopy and Open, and Neutral if there is not enought power to conduct the test,
#' stdres (standarized resdual quantifying the difference between the observed
#' and expected values), and
#' testability (indicating whether the sample size allow to conduct or not the test
#' being Non-testable those rows in which int_sign = Neutral).
#' The second, third and fourth elements of teh list are the matrices of only positive,
#' negative and neutral ( testable) associations based on the database generated in the first element of the list.
#'
#' @export
#'
#' @examples
#' sign_vest <- sign_net(RecruitNet, CanopyCover, "Ventisquero", "by_pairwise_interaction" )


sign_net <- function(inter_data, cover_data, site, type) {

  inter <- droplevels(RecruitNet[RecruitNet$Study_site == site, ])
  cover <- droplevels(CanopyCover[CanopyCover$Study_site == site, ])
  site <- unique(inter$Study_site)

if (type == "by_pairwise_interaction") { pre_index_all <- pre_asocindex(inter, cover)}
if (type == "by_recruit_sp") {pre_index_all <- recruit_level(inter, cover)}
if (type == "by_canopy_sp") {pre_index_all <- canopy_level(inter, cover)}


  pre_index_all$Frequency <- pre_index_all$Canopy_Freq

  db_inter <- sig_test(pre_index_all)

  db_inter$fij <- db_inter$Canopy_Freq

  pos <- droplevels(db_inter[db_inter$testability == "Positive", ])
  neg <- droplevels(db_inter[db_inter$testability == "Negative", ])
  neu <- droplevels(db_inter[db_inter$testability == "Neutral", ])

  cov_pos <- droplevels(cover[cover$Canopy %in% unique(pos$Canopy), ])
  cov_neg <- droplevels(cover[cover$Canopy %in% unique(neg$Canopy), ])
  cov_neu <- droplevels(cover[cover$Canopy %in% unique(neu$Canopy), ])

  posRN <- comm_to_RN(pos, cov_pos, unique(pos$Study_site))
  negRN <- comm_to_RN(neg, cov_pos, unique(neg$Study_site))
  neuRN <- comm_to_RN(neu, cov_pos, unique(neu$Study_site))

  pos_net <- RN_to_matrix(posRN[, c("Canopy", "Recruit", "fij")])
  pos_net <- pos_net[rowSums(pos_net) > 0, colSums(pos_net) > 0]

  neg_net <- RN_to_matrix(negRN[, c("Canopy", "Recruit", "fij")])
  neg_net <- neg_net[rowSums(neg_net) > 0, colSums(neg_net) > 0]

  neu_net <- RN_to_matrix(neuRN[, c("Canopy", "Recruit", "fij")])
  neu_net <- neu_net[rowSums(neu_net) > 0, colSums(neu_net) > 0]


  sign_networks <- list()

  sign_networks[[1]] <- db_inter
  sign_networks[[2]] <- pos_net
  sign_networks[[3]] <- neg_net
  sign_networks[[4]] <- neu_net

  names(sign_networks)[1] <- "All_interactions_db"
  names(sign_networks)[2] <- "Positive_interactions"
  names(sign_networks)[3] <- "Negative_interactions"
  names(sign_networks)[4] <- "Neutral_interactions"

  return(sign_networks)
}
