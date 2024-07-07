#' Function merge_RN_cover
#'
#' This function makes a data frame that merges the information from the recruitment network
#' and species cover for a local community.
#'
#' @param RN_data A data frame created with local_comm, containing all the
#' available data from a local community.
#' @param cover_data data set where cover data are stored. Usually, it will be "CanopyCover"
#' (or the name assigned to the dataset "CanopyCover.csv" when it was imported).
#'
#' @return A data.frame containing 5 columns with all the information needed
#' for the basic analysis of recruitment networks and canopy-recruit interactions:
#' canopy species (canopy), recruit species (recruit), recruitment frequency (fij),
#' cover of the canopy (cj) and cover of the recruit (ci).
#'
#' @examples
#' Ventisquero_RNc <- merge_RN_cover(Ventisquero_RN, Ventisquero_cover)

merge_RN_cover <- function(RN_data, cover_data) {

  # Find species present in RN but that lack data on cover.
  cover_list <- sort(unique(cover_data$Canopy))
  RN_list <- sort(unique(c(RN_data$Canopy, RN_data$Recruit)))
  lack_cover <- setdiff(RN_list, cover_list)

  # Remove species lacking cover from RN
  RNc <- if (length(which(RN_data$Recruit %in% lack_cover)) > 0) {
    RNc <- RN_data[-which(RN_data$Recruit %in% lack_cover), ]
  } else {
    RN_data
  }
  RNc <- if (length(which(RNc$Canopy %in% lack_cover)) > 0) {
    RNc <- RNc[-which(RNc$Canopy %in% lack_cover), ]
  } else {
    RNc
  }

  # Add variables with the cover of the canopy (cj) and recruit (ci) species
  RNc$cj <- RNc$Canopy
  RNc$ci <- RNc$Recruit
  for (i in 1:dim(RNc[1])) {
    RNc$cj[i] <- as.numeric(replace(
      RNc$Canopy[i],
      match(RN_list, RNc$Canopy[i]),
      cover_data$abundance[match(RNc$Canopy[i], cover_data$Canopy)]
    ))
  }

  for (i in 1:dim(RNc[1])) {
    RNc$ci[i] <- as.numeric(replace(
      RNc$Recruit[i],
      match(RN_list, RNc$Recruit[i]),
      cover_data$abundance[match(RNc$Recruit[i], cover_data$Canopy)]
    ))
  }

  RNc <- type.convert(RNc, as.is = TRUE)

  return(RNc)

}
