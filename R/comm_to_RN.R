#' Combine cover data with interaction data.
#'
#' @description
#' Combine interactions and cover data sets from a study site into a single data
#' frame. For study sites where data has been collected in several plots, this
#' function collapses the information into a single value per pairwise interaction.
#' You can choose whether or not the output contains all possible pairs of
#' interacting species (including those that were not observed, which are
#' assigned weight = 0) and whether or not to remove species lacking information
#' on canopy cover.

#' @inheritParams check_interactions
#' @inheritParams check_cover
#'
#' @param expand
#' Indicates whether to expand the dataframe to include the non-detected
#' interactions between every pair of species that occurs in the study site,
#' assigning values of 0 to their frequency. It can take two possible values:
#'   - *yes*: Generates all possible interactions between canopy and recruit
#'   species in the study site (i.e. detected and non-detected), adding values
#'   of interaction strength of 0 to the non-detected interactions.
#'   - *no*: Considers only the pairwise interactions that were actually
#'   detected in the study site.
#'
#' @param rm_sp_no_cover
#' Defines the filtering behavior, specifying which species should be removed
#' based on the availability of cover data. It can take two options:
#'   - *allsp*: removes all species lacking cover data (both canopy and recruit
#'   species).
#'   - *onlycanopy*: removes only canopy species lacking cover data, retaining
#'   recruit species even if they lack cover data.
#'
#' @returns Data frame with the following information for each pair of species:
#' - *Canopy*. Name of the canopy species, including "Open" interspaces. If you
#' use full scientific names, concatenate the epithets with a lower dash (e.g.
#' Olea_europaea).
#' - *Recruit*. Name of the recruit species. If you use full scientific names,
#' concatenate the epithets with a lower dash (e.g. Olea_europaea).
#' - *Ac*: Area (in m^2^), (or m when relative cover is measured with transects)
#' occupied by the canopy species (or "Open" interspaces) in the total area
#' sampled in the study site.
#' - *Ar*. Area (in m^2^), (or m when relative cover is measured with transects)
#' occupied by the recruit species (observed as adults), in the total area
#' sampled in the study site.
#' - *Fcr*: frequency of recruitment, in number of recruits by canopy-recruit
#' pair.
#' - *Icr*: incidence of the interaction, as number of plots where it was
#' observed.
#' - *Pcr*: presence/absence (1/0) of the interaction in the study site. This
#' provides the "unweighted" version of the adjacency matrix.
#'
#'
#' @export
#'
#' @examples
#' df <- comm_to_RN(Amoladeras_int, Amoladeras_cover, expand="yes",
#' rm_sp_no_cover="allsp")
#' head(df)
#'

comm_to_RN<- function(int_data,cover_data, expand=c("yes","no"), rm_sp_no_cover=c("allsp","onlycanopy")){


  if(expand=="yes" & rm_sp_no_cover=="allsp"){int_type ="rec"}
  if(expand=="yes" & rm_sp_no_cover=="onlycanopy"){int_type ="comp"}
  if(expand=="no" & rm_sp_no_cover=="onlycanopy"){int_type ="fac"}
  if(expand=="no" & rm_sp_no_cover=="allsp"){int_type ="unused"}


  if(int_type=="rec"){
    df<-comm_to_RN_UNI(int_data,cover_data)
  }

  if(int_type=="fac"){
    df<-comm_to_RN_BI(int_data,cover_data)
  }

  if(int_type=="comp"){
    df<-comm_to_RN_UNI_COMP(int_data,cover_data)
  }


  if(int_type=="unused"){

    df<-NULL
    warning("The object is not provided because the combination of arguments is not commonly used in plant-plant interaction networks")
  }

  return(df)


}


