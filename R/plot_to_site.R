#' TODO: title
#'
#' Convert plots into community data per site (either for a single or multiple sites)
#'
#' @param inter_data data frame with at least four columns:
#' Study_site (unique name of the study site),
#' Recruit (species of the recruit), Canopy (species of the canopy, or "Open"),
#' and Frequency (number of recruits of that species observed under that
#' canopy species in any plot, with "Open" representing recruits observed
#' without a canopy species).
#'
#' @param cover_data data frame with at least five columns:
#' Study_site (unique name of the study site),
#' Plot (unique name of each plot within a study site),
#' Canopy (species of the canopy),
#' Cover (percentage of cover of that species in that plot), and
#' Sampled_distance_or_area (total area of that plot or length in the case of transects).
#'
#' @return a list with two elements. The first element is a data frame with
#' the same structure as the input inter_data, but with two additional columns:
#' inter_ID (a unique identifier for each pair-wise interaction,
#' combining Study_site, Recruit, and Canopy species) and
#' Freq (the number of recruits of that species under that canopy species in
#' the entire study site).
#' The second element is a data frame with one row per canopy species in
#' each Study_site, including two new variables:
#' Canopy_cover (the percentage of cover of that species in the entire
#' sampled area of the Study_site) and mycover_sp (a unique identifier
#' for each canopy species in each Study_site, combining Study_site and Canopy species).
#'
#' @noRd
#'
#' @examples
#' all_nets <- plot_to_site(RecruitNet, CanopyCover)



plot_to_site <- function(inter_data = RecruitNet,
                        cover_data = CanopyCover
                        ) {
  data <- inter_data
  dbcover <- cover_data
  data$inter_ID <- paste(data$Study_site, data$Recruit, data$Canopy, sep = "_")

  options(dplyr.summarise.inform = FALSE)

  data$inter_ID <- paste(data$Study_site, data$Recruit, data$Canopy, sep = "_")
  dbcover2 <- droplevels(dbcover[!is.na(dbcover$Cover), ])
  rmnets <- setdiff(dbcover$Study_site, dbcover2$Study_site)

  #Those study_site that does not
  #have information about the canopy of any of its species will be removed

  dbcover <- dbcover[dbcover$Study_site %in% setdiff(dbcover$Study_site, rmnets), ]


  data <- data[data$Study_site %in% setdiff(data$Study_site, rmnets), ]


  #check if the pruning of the study sites, if required, have worked properly i.e. now all the study sites have at least one canopy species
  # with information about its cover (if not stop the process)

  if (!(length(setdiff(dbcover$Study_site, data$Study_site)) == 0 |
        length(setdiff(data$Study_site, dbcover$Study_site)) == 0))
    stop("Study_site with complete information in the two arguments x,y does not match")

  ###############

  dbcover$Canopy_cover_abs <- with (dbcover, (Cover * Sampled_distance_or_area) /
                                      100)

  surfN <- data.frame(
    dbcover |>
      dplyr::group_by(Study_site, Canopy) |>
      dplyr::summarise(Canopy_cover_abs = sum(Canopy_cover_abs, na.rm = TRUE))
  )


  site_surf <- data.frame(
    unique(dbcover[, c("Study_site", "Plot", "Sampled_distance_or_area")]) |>
      dplyr::group_by(Study_site) |>
      dplyr::summarise(Plot_sup = sum(Sampled_distance_or_area))
  )



  Canopy_all <- merge(surfN, site_surf, by = "Study_site")
  Canopy_all$Canopy_cover <- with(Canopy_all, (Canopy_cover_abs * 100) / Plot_sup)
  Canopy_all <- Canopy_all[, c("Study_site", "Canopy", "Canopy_cover")]

  Canopy_all[Canopy_all$Canopy_cover == 0, "Canopy_cover"] <- "NA"

  Canopy_all$Canopy_cover <- as.numeric(Canopy_all$Canopy_cover)
  data$mynet_sp <- paste(data$Study_site, data$Canopy, sep = "-")
  Canopy_all$mycover_sp <- paste(Canopy_all$Study_site, Canopy_all$Canopy, sep = "-")

  Canopy_all <- Canopy_all[!is.na(Canopy_all$Canopy_cover), ]


  rmnets_anyNA <- unique(data[data$mynet_sp %in% setdiff(unique(data$mynet_sp), (Canopy_all$mycover_sp)), "Study_site"])

  dbcover <- droplevels(dbcover[dbcover$Study_site %in% setdiff(dbcover$Study_site, rmnets_anyNA), ])
  data <- droplevels(data[data$Study_site %in% setdiff(data$Study_site, rmnets_anyNA), ])
  Canopy_all <- droplevels(Canopy_all[Canopy_all$Study_site %in% setdiff(Canopy_all$Study_site, rmnets_anyNA), ])


  inter <- data.frame(
    data |>
      dplyr::group_by(Study_site, Recruit, Canopy, inter_ID) |>
      dplyr::summarise(inter_ID = unique(inter_ID), Freq = sum(Frequency))
  )


  for (i in 1:length(unique(inter$Study_site)))

  {
    mysite <- inter[inter$Study_site == unique(inter$Study_site)[i], ]
    #if(!"Open"%in%unique(mysite$Canopy))

    myadj <- data[data$Study_site == unique(Canopy_all$Study_site)[i], ]

    Freq <- data.frame(myadj |>
                         dplyr::group_by(inter_ID) |>
                         dplyr::summarise(Freq = sum(Frequency)))

    alledge <- merge(Freq, unique(myadj[, c("Study_site", "inter_ID", "Canopy", "Recruit")]), by =
                       "inter_ID")
    myadj <- reshape2::dcast(data = alledge[, c("Recruit", "Canopy", "Freq")], Recruit ~
                               Canopy, value.var = "Freq")
    myadj2 <- as.matrix(myadj[, -1])
    colnames(myadj2) <- colnames(myadj)[-1]
    rownames(myadj2) <- myadj[, 1]
    myadj <- myadj2
    myadj2 <- rm
    myadj[is.na(myadj)] <- 0
    mycovs <- Canopy_all[Canopy_all$Study_site == unique(Canopy_all$Study_site)[i], ]
    myadj <- myadj[, colnames(myadj) %in% mycovs$Canopy]



    if (length(colnames(myadj)[which(colnames(myadj) == "Open")]) < 1) {
      myadj <- cbind(myadj, rep(0, dim(myadj)[1]))
      colnames(myadj)[dim(myadj)[2]] <- "Open"
    }


  }


  Canopy_all <- Canopy_all |> dplyr::arrange(Study_site)
  inter <- inter |> dplyr::arrange(Study_site)

  out <- list()
  inter$Frequency <- inter$Freq
  out[[1]] <- inter
  out[[2]] <- Canopy_all

  return(out)
}
