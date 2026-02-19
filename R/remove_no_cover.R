#' Remove species with no cover data
#'
#' @description
#' Remove the species from the interaction database that do not have cover data. For rec
#' interaction type the function removes any species without cover data from the
#' interactions database, and for comp or fac, it removes only canopy species without
#' cover data keeping recruit species without cover data.
#'
#' @inheritParams check_interactions
#' @inheritParams check_cover
#'
#' @param rm_sp_no_cover Options:
#' - *allsp*: removes any species without cover data.
#' - *onlycanopy*: removes only canopy species without cover.
#'
#' @returns a data frame
#'
#' @export
#'
#' @examples
#' remove_no_cover (Amoladeras_int, Amoladeras_cover, rm_sp_no_cover = "allsp")
#' remove_no_cover (Amoladeras_int, Amoladeras_cover, rm_sp_no_cover = "onlycanopy")
#'
remove_no_cover<- function(int_data,cover_data, rm_sp_no_cover=c("allsp","onlycanopy")){

  rm_sp_no_cover <- match.arg(rm_sp_no_cover)

  if(rm_sp_no_cover=="allsp"){
    df<-remove_no_cover_UNI(int_data,cover_data)
  }

  if(rm_sp_no_cover=="onlycanopy"){
    df<-remove_no_cover_BI(int_data,cover_data)
  }

  return(df)
}
