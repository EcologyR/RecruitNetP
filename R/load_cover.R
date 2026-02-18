#' Load canopy cover data from RecruitNet database
#'
#' @param path Path to folder containing the 'CanopyCover.csv' file. If the file
#' is not present, the latest version of the RecruitNet database (Verdú et al. 2023,
#' \doi{doi:10.1002/ecy.3923}) will be downloaded automatically.
#'
#' @returns a data frame containing canopy cover data
#' @export
#'
#' @examples
#' int_data <- load_cover()
load_cover <- function(path = getwd()) {

  if (!file.exists(file.path(path, "CanopyCover.csv"))) {
    download_RN(path)
  }

  CanopyCover <- read.csv(file.path(path, "CanopyCover.csv"))

  return(CanopyCover)

}
