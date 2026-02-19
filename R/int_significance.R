#' Title
#'
#' @param int_data
#' @param cover_data
#' @param int_type
#'
#' @returns
#' @export
#'
#' @examples
#'
#' # int_significance() Use all functions to consider all interactions (observed and non-observed) (for Recruitment networks) or only observed interactions (for facilitation/completition networks)
#'
#'
int_significance <- function(int_data, cover_data, int_type=c("rec", "fac","comp")){

  if (!"Open" %in% int_data$Canopy) stop("ERROR: tests cannot be conducted because your data does not contain a node named Open or it is spelled differently.")

  if(int_type=="rec"){

    data<-comm_to_RN_UNI(int_data,cover_data)
    df<-int_significance_UNI(data)
    if(length(unique(df$Test_type))>1) message("Different tests were used for different canopy-recruit pairs. Check column Test_type")
  }

  if(int_type=="fac"){

    data<-comm_to_RN_BI(int_data,cover_data)
    df<-int_significance_BI (data)
    df<-df[df$Effect_int=="Enhancing",]
    if(length(unique(df$Test_type))>1) message("Different tests were used for different canopy-recruit pairs. Check column Test_type")
  }

  if(int_type=="comp"){

    data<-comm_to_RN_UNI_COMP(int_data,cover_data)
    df<-int_significance_BI_COMP(data)
    df<-df[df$Effect_int=="Depressing",]
    if(length(unique(df$Test_type))>1) message("Different tests were used for different canopy-recruit pairs. Check column Test_type")

  }

  return(df)
}


