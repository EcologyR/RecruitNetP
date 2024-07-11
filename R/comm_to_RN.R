#' Merge cover data with interaction data for a given site
#'
#' This function makes a data frame that merges the information from the
#' recruitment network and species cover for a local community.
#' It is a shortcut to directly obtain the data for a local community
#' from the RecruitNet dataset.
#'
#' @param RN_data data set where recruitment networks are stored.
#' Usually, it will be "RecruitNet" (or the name assigned to the dataset when it was imported).
#'
#' @param cover_data ndata set where cover data is stored.
#' Usually, it will be "CanopyCover" (or the name assigned to the dataset
#' "CanopyCover.csv" when it was imported).
#'
#' @param site name of the study site.
#'
#' @return A data.frame containing 5 columns with all the information needed
#' for the basic analysis of recruitment networks and canopy-recruit interactions:
#' canopy species (canopy), recruit species (recruit), recruitment frequency (fij),
#' cover of the canopy (cj) and cover of the recruit (ci).
#'
#' @export
#'
#' @examples
#' Ventisquero_RNc <- comm_to_RN(RecruitNet, CanopyCover, site = "Ventisquero")

comm_to_RN <- function(RN_data, cover_data, site) {

  merge_RN_cover(aggr_RN(comm_subset(RN_data, site)), aggr_cover(cover_data, site))

}
