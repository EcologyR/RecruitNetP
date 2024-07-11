#' Link_completeness
#'
#' Observed and estimated number of links, and link completeness.
#' Methods based in Chao et al. (2014) as implemented in R package iNEXT.
#'
#' @param dataset The name of the data set where recruitment networks are stored.
#' Usually, it will be "RecruitNet" (or the name assigned to the data set when
#' it was imported). At least, the data set must contain columns named
#' "Study_site", "Plot", "Canopy", "Recruit" and "Frequency".
#' In data sets containing a single site or a single plot,
#' the corresponding columns must be included anyway.
#'
#' @param site The name of a study site.
#' @param type Can take two values: "incidence" or "abundance".
#' Estimates based on incidence data are recommended, but can only be obtained
#' if your data set is structured in multiple plots. Alternatively, for data
#' collected in a single plot, estimates can be based only on abundance data.
#'
#' @export
#'
#' @return A data frame TODO
#'
#' @examples
#' link_completeness(RecruitNet, "Ventisquero", "incidence")
#' link_completeness(RecruitNet, "Laxe", "abundance")
#' link_completeness(RecruitNet, "Laxe", "incidence") # Issues a warning
#' link_completeness(RecruitNet, "LosReyes", "abundance")
#' link_completeness(RecruitNet, "LosReyes", "incidence") #Issues an error


link_completeness <- function(dataset = NULL,
                              site = NULL,
                              type = c("incidence", "abundance")) {

  stopifnot(
    c("Study_site",
      "Plot",
      "Canopy",
      "Recruit",
      "Frequency"
    ) %in% names(dataset))

  stopifnot(is.character(site))
  stopifnot(length(site) == 1)

  type <- match.arg(type)


  data_raw <- data.frame(dataset[dataset$Study_site %in% site, ])
  data_RN <- aggr_RN(data_raw)

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

    # Combine the lists of canopy and recruit species to obtain the total list of species sampled.

    a1 <- split(netRaw[-1], f = netRaw[1])
    a2 <- lapply(a1, unique)
    a3 <- unlist(unlist(a2, recursive = FALSE, use.names = FALSE))

    # Table showing the incidence of each species in the study site

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
    df <- data.frame(c(Lobs, Lest, Cq0_L, Cq1_L))
    colnames(df) <- c("Incidence based estimate")
    rownames(df) <- c("Lobs",
                      "Lest",
                      "Completeness Links (q=0)",
                      "Coverage Links (q=1)")
  }

  # Completeness based on abundance or frequency of recruits.

  if (type == "abundance") {
    # Call to iNEXT to obtain completeness values

    out <- iNEXT::iNEXT(data_RN$fij[which(data_RN$fij > 0)], q = 0, datatype = "abundance")
    Lobs <- out$AsyEst[1, 1]
    Lest <- out$AsyEst[1, 2]
    Cq0_L <- Lobs / Lest
    Cq1_L <- out$DataInfo[1, 4]
    df <- data.frame(c(Lobs, Lest, Cq0_L, Cq1_L))
    colnames(df) <- c("Abundance based estimate")
    rownames(df) <- c("Lobs",
                      "Lest",
                      "Completeness of links (q=0)",
                      "Coverage of links (q=1)")
  }

  return(df)
}
