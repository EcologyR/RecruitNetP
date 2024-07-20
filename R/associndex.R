

#' Calculate interaction strength indices
#'
#' @param int_data data frame with at least four columns:
#' - Study_site (unique name of the study site),
#' - Recruit (species of the recruit),
#' - Canopy (species of the canopy, or "Open"), and
#' - Frequency (number of recruits of that species observed under that
#'   canopy species in any plot, with "Open" representing recruits observed
#'   without a canopy species).
#' If available, the dimensions of the study plots can be also included as
#' variables PlotdimX and PlotdimY.
#' @param cover_data data frame with at least five columns:
#' - Study_site (unique name of the study site),
#' - Plot (unique name of each plot within a study site),
#' - Canopy (species of the canopy),
#' - Cover (percentage of cover of that species in that plot), and
#' - Sampled_distance_or_area (total area of the plot).
#' @param area_sampled Total area surveyed in the study site (in m2).
#' @param thr_dens Maximum density considered as reasonable. For example, if a canopy species is rare
#' (e.g. 0.01 m2) but there is one sapling growing beneath, we would estimate a density of 100 saplings/m2
#' for this interaction, what is likely an overestimate because it seems reasonable that increasing the
#' effort under this canopy species we would obtain a much lower density estimate. The threshold is set
#' by default to 100 recruits/m2.
#'
#' @return data frame with indices of pair-wise interaction strength and the data neeeded for their estimation.
#' Each row corresponds to each pair-wise interaction, and contains the following columns:
#' Recruit: recruit species.
#' Canopy: canopy species.
#' inter_ID: a unique identifier for each pair-wise interaction, combining Study_site, Recruit, and Canopy species.
#' Study_site: name of the study site.
#' Canopy_Freq: number of recruits found in the vicinity of the canopy species.
#' Open_Freq: number of recruits found in open spaces.
#' Canopy_cover: percentage of the total area sampled in the study site occupied by the canopy species.
#' Open_cover: percentage of the total area sampled in the study site occupied by Open spaces.
#' Freq_tot: sum of Canopy_Freq and Open_Freq (total number of recruits of the recruit species in the area sampled
#' in the study site.
#' RecrDens_canopy: density of recruits under the canopy species (recruits/m2).
#' RecrDens_open: density of recruits in open spaces (recruits/m2).
#' Max_Recr_Density: maximum recruitment density between under the canopy species vs in open spaces.
#' max_Recr: maximum recruitment density of the recruit species in the study site.
#' Ns: Normalized Neighbour Suitability index (Mingo, 2014).
#' NintC: commutative symmetry intensity index (Díaz-Sierra et al., 2017)
#' NintA:additive symmetry intensity index (Díaz-Sierra et al., 2017)
#' RII: Relative Interaction Index (Armas et al., 2004).
#'
#' @export
#'
#' @examples
#' data(RecruitNet)
#' data(CanopyCover)
#' int_data <- comm_subset(RecruitNet, site = "Ventisquero")
#' cover_data <- CanopyCover[CanopyCover$Study_site == "Ventisquero", ]
#' Ventisquero_int <- associndex(int_data, cover_data)
#'

associndex <- function(int_data = NULL,
                       cover_data = NULL,
                       area_sampled = NULL,
                       thr_dens = 100) {

  if (is.null(area_sampled) & !"PlotdimX" %in% names(int_data)) {
    stop("ERROR: sampled area cannot be calculated with the data available. You must enter a value in area_sampled or add variables PlotdimX and PlotdimY in your data.")
  }

  if (is.null(area_sampled)) {
    area_sampled = comm_summary(int_data)["Area sampled (m2)"][1,1]
  }

  threshold_density <- thr_dens

  # Assemble the data
  db_inter <- pre_asocindex(int_data, cover_data)

  # Incorporate density of recruitment (recruits/m2) under each canopy species and in open.
  db_inter$RecrDens_canopy <- db_inter$Canopy_Freq/((db_inter$Canopy_cover/100)*area_sampled)
  db_inter$RecrDens_open <- db_inter$Open_Freq/((db_inter$Open_cover/100)*area_sampled)

  # remove the interactions with estimated density above the threshold.
  db_inter <- db_inter[which(db_inter$RecrDens_canopy<threshold_density), ]
  db_inter <- db_inter[which(db_inter$RecrDens_open<threshold_density), ]

  #Obtain the maximum recruitment density for each recruit under the canopy species or in open.
  db_inter$Max_Recr_Density <- pmax(db_inter$RecrDens_canopy,db_inter$RecrDens_open)

  db_inter <- utils::type.convert(db_inter, as.is = TRUE)

  max_rd <- stats::aggregate(Max_Recr_Density ~ Recruit, data = db_inter, FUN = "max")
  # Add a variable max_Recr to each pair indicating the maximum recruitment density of the recruit species in the study site
  Recr_list <- sort(unique(c(db_inter$Recruit)))
  Dens_list <- sort(unique(max_rd$Recruit))
  lack_dens <- setdiff(Recr_list, Dens_list)

  # Remove species lacking density from dataset (e.g. those with density estimates above the threshold)
  db_inter <- if (length(which(db_inter$Recruit %in% lack_dens)) > 0) {
    db_inter <- db_inter[-which(db_inter$Recruit %in% lack_dens), ]
  } else {
    db_inter
  }

  db_inter$max_Recr <- db_inter$Recruit
  for (i in 1:(dim(db_inter)[1])) {
    db_inter$max_Recr[i] <- replace(
      db_inter$max_Recr[i],
      match(Recr_list, db_inter$max_Recr[i]),
      max_rd$Max_Recr_Density[match(db_inter$max_Recr[i], max_rd$Recruit)]
    )
  }

  db_inter <- utils::type.convert(db_inter, as.is = TRUE)

  # Calculate indices Ns, NintC, NintA and RII
  db_inter$Ns <- (db_inter$RecrDens_canopy - db_inter$RecrDens_open)/db_inter$max_Recr
  db_inter$NintC <- 2*(db_inter$RecrDens_canopy - db_inter$RecrDens_open)/((db_inter$RecrDens_canopy + db_inter$RecrDens_open)+abs(db_inter$RecrDens_canopy-db_inter$RecrDens_open))
  db_inter$NintA <- 2*(db_inter$RecrDens_canopy - db_inter$RecrDens_open)/((db_inter$RecrDens_open) + abs(db_inter$RecrDens_canopy-db_inter$RecrDens_open))
  db_inter$RII <- (db_inter$RecrDens_canopy - db_inter$RecrDens_open)/(db_inter$RecrDens_canopy + db_inter$RecrDens_open)

  removed <- names(db_inter) %in% c("Frequency")
  db_inter <- db_inter[!removed]
  return(db_inter)

}
