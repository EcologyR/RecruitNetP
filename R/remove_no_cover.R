#' Title
#'
#' @param int_data
#' @param cover_data
#' @param rm_sp_no_cover
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
# remove_no_cover : remove the species from the interaction database that do not have cover data
#rm_sp_no_cover:
#"allsp" removes any species without cover data,
#"onlycanopy":removes only canopy species without cover

#for rec interaction type remove any species without cover data from
#the interactions database, and for comp or fac remove only canopy species without cover data
#keeping recruit speceis without cover data
#'
remove_no_cover<- function(int_data,cover_data, rm_sp_no_cover=c("allsp","onlycanopy")){

  if(rm_sp_no_cover=="allsp"){
    df<-remove_no_cover_UNI(int_data,cover_data)
  }

  if(rm_sp_no_cover=="onlycanopy"){
    df<-remove_no_cover_BI(int_data,cover_data)
  }

  return(df)
}
