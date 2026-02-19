#' Load interaction data from RecruitNet database
#'
#' @param path Path to folder containing the 'RecruitNet.csv' file. If the file
#' is not present, the latest version of the RecruitNet database (Verdú et al. 2023,
#' \doi{doi:10.1002/ecy.3923}) will be downloaded automatically.
#'
#' @returns a data frame containing interaction data
#' @export
#'
#' @examples
#' int_data <- load_interactions()
load_interactions <- function(path = getwd()) {

  if (!file.exists(file.path(path, "RecruitNet.csv"))) {
    download_RN(path)
  }

  RecruitNet <- utils::read.csv(file.path(path, "RecruitNet.csv"))

  return(RecruitNet)

}




