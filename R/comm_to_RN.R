#' Title
#'
#' @param int_data
#' @param cover_data
#' @param expand
#' @param rm_sp_no_cover
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
#' # comm_to_RN: summarizes data collected by plot to the community level ( merging plots)
#expand:expands non observed interctions
#rm_sp_no_cover: "allsp" removes any species without cover data, while "onlycanopy":removes only canopy species without cover

#that defines internaly the combination of those arguments usually used to work with different interaction types
#rec: expands non observed interctions and removes any species without cover data (canopy or recruit)
#fac: considers only observed intearctions and removes only canopy species without cover data (keeping recruit species without cover)
#comp: expand non observed interactions and removes canopy species without cover data

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


