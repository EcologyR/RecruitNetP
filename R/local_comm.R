#' Function local_comm
#'
#' This function extracts the information of a given local community and
#' stores it in a separate data frame. It can also extract more than one.
#'
#' @param dataset data set where recruitment networks are stored.
#' When using the RecruitNet database, this name will be "RecruitNet",
#' but can be any name assigned to the dataset when it is imported.
#'
#' @param site name of a study site within the dataset or a character vector
#'  with the names of several study sites.
#'
#' @return A data frame containing all the data from the desired community/ies
#'
#' @examples
#' #Ventisquero_raw <- local_comm(RecruitNet, "Ventisquero")
#'
#' #TwoSites <- c("Ventisquero","Agadir")
#' #TwoSites_raw <- local_comm(RecruitNet, TwoSites)

local_comm <- function(dataset, site) {

  data_set <- data.frame(dataset[dataset$Study_site %in% site, ])

}
