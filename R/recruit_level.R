#' TODO: title
#'
#' Calculate in separate columns the total number of recruits of each species
#' under any species of canopy and in the open and its respective percentage
#' of cover of vegetation and Open (either for multiple or a single site)
#'
#' @param inter_data data frame with at least four columns:
#' Study_site (unique name of the study site),
#' Recruit (species of the recruit),
#' Canopy (species of the canopy, or "Open"), and
#' Frequency (number of recruits of that species observed under
#' that canopy species in any plot, with "Open" representing recruits observed
#' without a canopy species).
#'
#' @param cover_data data frame with at least five columns:
#' Study_site (unique name of the study site),
#' Plot (unique name of each plot within a study site),
#' Canopy (species of the canopy),
#' Cover (percentage of cover of that species in that plot), and
#' Sampled_distance_or_area (total area of that plot or length in the case of transects).
#'
#' @return data frame with one row per Rercuit species in each site and nine columns:
#' inter_ID (a unique identifier for each pair-wise interaction,
#' combining Study_site, Recruit, and Canopy species),
#' Recruit(recruit species),
#' Canopy (canopy speices),
#' Study_site (name of the study site),
#' Canopy_Freq and Open_Freq with the number of recuits observed
#' under that canopy species or in the Open, respectively and Canopy_cover and
#' Open_cover, with the percentage of the total area sampled in the Study_site
#' occupied by that canopy species and Open respectively.
#' Freq_tot is the sum of Canopy_Freq and Open_Freq (total number of recruits
#' of that speices observed in the area sampled in the Study_site).
#'
#' @export
#'
#' @examples
#' #db_sp_rec<-recruit_level (RecruitNet, CanopyCover)

recruit_level <- function(inter_data = RecruitNet,
                          cover_data = CanopyCover
                          ) {

  options(dplyr.summarise.inform = FALSE)

  data <- inter_data
  dbcover <- cover_data

  db_inter <- pre_asocindex(data, dbcover)

  db_sp_rec <- data.frame(
    db_inter |>
      dplyr::group_by(Study_site, Recruit) |>
      dplyr::summarise(
        Canopy_Freq = sum(Canopy_Freq, na.rm = TRUE),
        Open_Freq = unique(Open_Freq, na.rm = TRUE),
        Open_cover = unique(Open_cover, na.rm = TRUE),
        Canopy_cover = 100 - unique(Open_cover, na.rm = TRUE)
      )
  )

  return(db_sp_rec)

}
