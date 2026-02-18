#' Subset sites CAMBIO
#'
#' This function extracts the information of a given local community and
#' stores it in a separate data frame. It can also extract more than one site.
#'
#' @param dataset A data frame containing recruitment data.
#'
#' @param site character. Name of a study site within the dataset or
#' a character vector with the names of several study sites.
#'
#' @return A data frame containing all the data from the desired community/ies
#'
#' @export
#'
#' @examples
#' data(RecruitNet)
#'
#' Ventisquero <- comm_subset(RecruitNet, site = "Ventisquero")
#'
#' TwoSites <- comm_subset(RecruitNet, c("Ventisquero","Agadir"))

comm_subset <- function(dataset = NULL, site = NULL) {

  stopifnot("Study_site" %in% names(dataset))

  return(dataset[dataset$Study_site %in% site, ])

}
