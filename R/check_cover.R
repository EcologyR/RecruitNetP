#' Check cover data format
#'
#' @description
#' The **canopy cover data set** contains the abundance of each canopy species in each plot. The minimum columns required are:
#' - **Plot**. As in the interactions data set.
#' - **Canopy**. As in the interactions data set.
#' - **Cover**. Cover of the canopy species (and "Open" interspaces), measured as % of the total area sampled where a recruit would be ascribed to the interaction with the canopy plant (or to be recruiting in "Open", away from established plants). For example, following Alcantara et al. (2019), in plants with branches less than 1.5m above ground (e.g. small shrubs and treelets), it would be the area of projection of their canopy on the ground, while in tall trees it can be the area extending 0.5m around the trunk's base or large surfacing roots.
#' - **Sampled_distance_or_area**. Total area of each plot or distance of each transect (in m^2^ or m, respectively).
#'
#' @param cover_data data frame with the abundance of each canopy species in each plot.
#'
#' @returns The function will return error(s) if problems are detected. Otherwise
#' an OK message.
#' @export
#'
#' @examples
#' cover_data <- load_cover()
#' check_cover(cover_data)
check_cover <- function(cover_data = NULL) {

  stopifnot(c("Plot","Canopy", "Cover", "Sampled_distance_or_area") %in% names(cover_data))

  cover_data <- cover_data[, c("Plot","Canopy", "Cover", "Sampled_distance_or_area")]

  if (anyNA(cover_data)) stop("There cannot be NA.")

  stopifnot(is.character(cover_data$Plot))
  stopifnot(is.character(cover_data$Canopy))
  if (grepl(" ", cover_data$Canopy)) {
    stop("There cannot be spaces in 'Canopy'")
  }
  if (!"Open" %in% cover_data$Canopy) {
    stop("Open must be present in 'Canopy'. If there is no Open habitat, assign Cover value = 0")
  }

  stopifnot(is.numeric(cover_data$Cover))
  if (cover_data$Cover < 0) stop("Cover values cannot be negative.")

  stopifnot(is.numeric(cover_data$Sampled_distance_or_area))
  stopifnot(cover_data$Sampled_distance_or_area > 0)


  message("Format OK!")

}
