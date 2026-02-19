#' Estimates the completeness and coverage of the sampled interactions
#'
#' @description
#' Estimates the completeness and coverage of the sampled interactions,
#' providing the probability of detecting a new canopy-recruit pair if we
#' increased our sampling effort in one unit (i.e., locating one more recruit
#' or surveying one more plot). This function uses [iNEXT::iNEXT()]
#' (Hsieh et al. 2016).
#'
#'
#' @inheritParams check_interactions
#'
#' @param type indicates whether the completeness and coverage will be
#' calculated based on the number of individual recruits under each canopy
#' species (i.e., abundance) or on the presence/absence of canopy-recruit pairs
#' across plots (i.e., incidence). If the survey was based in multiple
#' independent plots, it is good practice to use the "incidence" approach. The
#' "abundance" approach can be used more generally, but it assumes that each
#' recruit represents an independent canopy-recruit interaction event, so it may
#' easily overestimate coverage when recruits of the same species tend to occur
#' aggregated under the same canopy plant (as it occurs frequently).
#' Explanation of its options:
#' - **incidence**: Uses the incidence of a given interaction as number of plots
#' where it was observed.
#' - **abundance**: Uses the abundance of a given interaction as the number of
#' recruits by canopy-recruit.
#'
#' @returns A data frame indicating the number of links observed (Lobs), the
#' number of links estimated (Lest), the completeness of the sampling of links
#' as the proportion of canopy-recruit pairs observed from those expected in
#' the study site (Completeness of links: Lobs/Lest) and the probability that
#' if one more recruit (or plot) had been sampled, it would have corresponded
#' to (or contained) an already detected canopy-recruit species interaction
#' (Coverage of links).
#'
#' @export
#'
#' @examples
#' link_completeness(Amoladeras_int, type="abundance")
#' link_completeness(Amoladeras_int, type="incidence")
#'
link_completeness <- function(int_data = NULL,
                              type = c("incidence", "abundance")) {

  stopifnot(
    c("Plot",
      "Canopy",
      "Recruit",
      "Frequency"
    ) %in% names(int_data))

  type <- match.arg(type)

  data_raw <- int_data
  data_RN <- aggr_RN_UNI(data_raw)

  # Completeness based on incidence data.

  if (type == "incidence") {
    netRaw <- data.frame(cbind(data_raw$Plot, paste(data_raw$Canopy, data_raw$Recruit)))
    colnames(netRaw) <- c("Plot", "Pair")
    nPlots <- length(unique(netRaw$Plot))

    # Check points.

    if (nPlots == 1)
      stop(
        "ERROR: your data is not structured in multiple plots. Incidence approach cannot be used. Try the abundance approach."
      )

    if (nPlots < 10)
      warning(
        "WARNING: your are using the incidence approach with very few plots. Consider using the abundance approach if appropriate."
      )

    # Combine the lists of canopy and recruit species to obtain the total list of canopy-recruit pairs (links) sampled.

    a1 <- split(netRaw[-1], f = netRaw[1])
    a2 <- lapply(a1, unique)
    a3 <- unlist(unlist(a2, recursive = FALSE, use.names = FALSE))

    # Table showing the incidence of each canopy-recruit pair in the study site

    a4 <- table(a3)
    linkIncidence <- as.data.frame(a4)
    colnames(linkIncidence) <- c("Pair", "Incidence")

    # Incidence list to be passed to iNEXT

    data_iNEXT <- c(nPlots, sort(linkIncidence$Incidence, decreasing = TRUE))

    # Call to iNEXT to obtain completeness values

    out <- iNEXT::iNEXT(
      data_iNEXT,
      q = c(0, 1),
      datatype = "incidence_freq",
      se = FALSE,
      size = nPlots
    )
    Lobs <- out$AsyEst[1, 1]
    Lest <- out$AsyEst[1, 2]
    Lest_LCL <- out$AsyEst[1, 4]
    Lest_UCL <- out$AsyEst[1, 5]
    Cq0_L <- Lobs / Lest
    Cq1_L <- out$DataInfo[1, 5]
    df <- data.frame(c(nPlots, Lobs, Lest, Cq0_L, Cq1_L))
    colnames(df) <- c("Incidence based estimate")
    rownames(df) <- c("Num. Plots sampled",
                      "Lobs",
                      "Lest",
                      "Completeness Links (q=0)",
                      "Coverage Links (q=1)")
  }

  # Completeness based on abundance or frequency of recruits.

  if (type == "abundance") {

    warning("Abundance-based approach assumes that each individual recruit provides independent data about the canopy-recruit interaction. If conspecific recruits frequently occur aggregated under individual canopy plants, the estimates of completeness and coverage may be severely overestimated."
    )

    # Call to iNEXT to obtain completeness values

    nPlots <- length(unique(int_data$Plot))

    if (nPlots > 9)
      warning(
        "Your data is structured in multiple plots. Incidence-based approach is recommended."
      )

    out <- iNEXT::iNEXT(data_RN$Fcr[which(data_RN$Fcr > 0)], q = 0, datatype = "abundance")
    nPlants <- out$DataInfo[1,2]
    Lobs <- out$AsyEst[1, 1]
    Lest <- out$AsyEst[1, 2]
    Cq0_L <- Lobs / Lest
    Cq1_L <- out$DataInfo[1, 4]
    df <- data.frame(c(nPlants, Lobs, Lest, Cq0_L, Cq1_L))
    colnames(df) <- c("Abundance based estimate")
    rownames(df) <- c("Num. plants (recruits) sampled",
                      "Lobs",
                      "Lest",
                      "Completeness of links (q=0)",
                      "Coverage of links (q=1)")
  }

  return(df)
}

