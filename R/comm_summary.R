#' Summarise metadata of a community
#'
#' This function provides a summary of the characteristics (i.e. metadata) of
#' a local community.
#'
#' @param dataset A data frame for a single site (or a group of sites).
#' For example, a data frame created with subset_comm. This function relies on
#' the variable names used in the original RecruitNet database, so it cannot be used
#' with other datasets unless they follow exactly the ordering and the names used in RecruitNet.
#'
#' @return A data frame with summary information of the local community/ies.
#'
#' @export
#'
#' @examples
#' All_sites_details <- comm_summary(RecruitNet)
#' Ventisquero_raw <- comm_subset(RecruitNet, site = "Ventisquero")
#' Ventisquero_details <- comm_summary(Ventisquero_raw)


comm_summary <- function(dataset) {

  dfList <- list()


  for (i in 1:length(unique(dataset$Study_site))) {

    data <- comm_subset(dataset, unique(dataset$Study_site)[i])

    # Plots data
    n_plots <- length(unique(data$Plot))
    plotDim <- data$PlotdimX[1]*data$PlotdimY[1]
    areaSampled <- plotDim*n_plots

    # Nodes data
    isOpen <- ifelse(is.element('Open', data$Standardized_Canopy) == TRUE, "Yes", "No")
    n_nodes <- length(unique(c(data$Standardized_Canopy, data$Standardized_Recruit)))
    n_sp <- ifelse(isOpen == "Yes", n_nodes-1,n_nodes)
    num_woody <- length(which(unique(data.frame(c(data$LifeHabit_Canopy, data$LifeHabit_Recruit),c(data$Standardized_Canopy, data$Standardized_Recruit)), margin=1) == "W"))
    num_herbs <- length(which(unique(data.frame(c(data$LifeHabit_Canopy, data$LifeHabit_Recruit),c(data$Standardized_Canopy, data$Standardized_Recruit)), margin=1) == "H"))
    num_others <- n_sp - num_woody - num_herbs

    # Function output
    df <- data.frame(c(data$Study_site[1], data$Country[1], data$Latitude[1], data$Longitude[1], data$Sampling_date[1], data$Site_responsible[1], data$Biome[1], data$Vegetation_type[1], data$Community[1], data$Successional_stage[1], data$Disturbance[1], data$Sampling_method[1], n_plots, plotDim, areaSampled, n_sp, isOpen, num_woody, num_herbs, num_others))
    colnames(df) <- c("Value")
    rownames(df) <- c("Local Community", "Country", "Latitude", "Longitude", "Year of sampling", "Site responsible", "Biome", "Vegetation", "Plant Community", "Successional stage", "Disturbance", "Sampling method", "Number of plots", "Plot area (m2)", "Area sampled (m2)", "Number of plant species", "Contains Open node", "Number of woody species", "Number of herb species", "Number of other types")

    dfList[[i]] <-  t(df)

    }

  dfAll <-  as.data.frame(do.call(rbind, dfList))

  dfAll <- utils::type.convert(dfAll, as.is = TRUE)

  return(dfAll)

}
