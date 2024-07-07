#' Calculate aggregated species cover
#'
#' Calculate the aggregated (i.e. summed) cover of each species across
#' all the plots from a local community.
#'
#' @param dataset character. Name of the data set where cover data are stored.
#' Usually, it will be "CanopyCover" (or the name assigned to the
#' dataset "CanopyCover.csv" when it was imported).
#' @param site character. Name of a study site.
#'
#' @return A data frame containing the total cover of each species in a local community,
#' aggregated across plots. It has two columns: "Canopy", with the name of each species (irrespective
#' of being acting as canopies or recruits); and "abundance".
#'
#' @examples
#' #Ventisquero_cover <- aggr_cover(CanopyCover, site = "Ventisquero")

aggr_cover <- function(dataset, site) {
  cover <- dataset[which(dataset$Study_site == site), ]
  cover$abundance <- (cover$Cover / 100) * cover$Sampled_distance_or_area
  cover <- stats::aggregate(abundance ~ Canopy, data = cover, sum)
  return(cover)

}
