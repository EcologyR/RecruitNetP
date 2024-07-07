#' Function split_local_comm
#'
#' When a local community has been sampled in multiple plots, we may need to
#' work with data from each plot separately. This function produces a list with
#' the basic data to build a RN for each plot.
#'
#' @param raw_RN A data frame containing all the data from a community sampled
#' in multiple plots. It must contain, at least, the columns "Plot", "Canopy",
#' "Recruit" and "Frequency".
#'
#' @return A list of data frames, each containing the frequency of
#' canopy-recruit interactions in a plot.
#'
#' @examples
#' #Ventisquero_plots <- split_local_comm(Ventisquero_raw)

split_local_comm <- function(raw_RN) {

  a <- data.frame(cbind(raw_RN$Plot, raw_RN$Canopy, raw_RN$Recruit, raw_RN$Frequency))
  colnames(a) <- c("Plot", "Canopy", "Recruit", "Frequency")
  a <- transform(a, Frequency = as.numeric(Frequency))
  a <- transform(a, Plot = as.factor(Plot))
  plot_list <- split(a, f = a$Plot)
  return(plot_list)

  }
