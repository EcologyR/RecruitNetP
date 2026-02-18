#' Download RecruitNet database from Zenodo
#'
#' Download latest version of the RecruitNet database (Verdú et al. 2023,
#' \doi{doi:10.1002/ecy.3923}).
#'
#' @param path character. Path to folder where the RecruitNet database should be saved.
#' @param destfile character. Name of the zip file to be saved.
#' @param unzip Logical. Uncompress the zip file? Default is TRUE.
#'
#' @return A zip file or two CSV files, depending if unzip is TRUE.
#' @export
#'
#' @examplesIf interactive()
#' download_RN()
#'

download_RN <- function(path = getwd(), destfile = "RN.zip", unzip = TRUE) {

  utils::download.file("https://zenodo.org/record/6567608/files/Data_S1.zip?download=1",
                destfile = file.path(path, destfile), mode = "wb")

  if (isTRUE(unzip)) {
    utils::unzip(file.path(path, destfile), exdir = path)
    file.remove(file.path(path, destfile))
  }

}
