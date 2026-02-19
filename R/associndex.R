#' Title
#'
#' @param int_data
#' @param cover_data
#' @param expand
#' @param rm_sp_no_cover
#' @param threshold_density
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
#Calculate different association indices considering different interaction types refer to:
#expand:expands non observed interactions (yes or no)
#rm_sp_no_cover: "allsp" removes any species without cover data, while "onlycanopy":removes only canopy species without cover
#'
associndex<- function(int_data, cover_data, expand=c("yes","no"), rm_sp_no_cover=c("allsp","onlycanopy"),
                      threshold_density=NULL)

{

  if (!"Open" %in% int_data$Canopy) stop("ERROR: your data does not contain a node named Open or it is spelled differently. Data for recruitment in Open is required to calculate the indices.")

  # Suggest a threshold value based on recruitment density outliers according to a Weibull distribution. Uses "extremevalues" package.
  int_data_0<-comm_to_RN_UNI(int_data, cover_data)
  db_inter_0 <- pre_associndex_UNISITE_UNI(int_data_0)
  db_inter_0$Dcr <- db_inter_0$Fcr/db_inter_0$Ac
  db_inter_0$Dro <- db_inter_0$Fro/db_inter_0$Ao
  y <- rbind(db_inter_0$Dcr,db_inter_0$Dro)
  y <- y[which(y>0)] # Zero densities are common, so we will check only for suspiciously high density values.
  K <- getOutliers(y, method = "I", distribution = "weibull", rho=c(1,1)) # We consider that a value is an outlier
  # if it is above the limit where less
  # than 1 observation is expected.

  if(is.null(threshold_density)){
    threshold_density = K$limit[[2]]
    message("Based on Weibull distribution fitted to the observed values, the threshold density has been set to: ", threshold_density, ".")
  }


  if(expand=="yes" & rm_sp_no_cover=="allsp"){int_type ="rec"}
  if(expand=="yes" & rm_sp_no_cover=="onlycanopy"){int_type ="comp"}
  if(expand=="no" & rm_sp_no_cover=="onlycanopy"){int_type ="fac"}
  if(expand=="no" & rm_sp_no_cover=="allsp"){int_type ="unused"}

  if(int_type=="rec"){

    int_data<-comm_to_RN_UNI(int_data, cover_data)
    db_inter<-associndex_UNISITE_UNI(int_data, threshold_density)

  }

  if(int_type=="fac"){

    int_data<-comm_to_RN_BI(int_data, cover_data)
    db_inter<-associndex_UNISITE_BI(int_data, threshold_density)

  }

  if(int_type=="comp"){

    int_data<-comm_to_RN_UNI_COMP(int_data, cover_data)
    db_inter<-associndex_UNISITE_BI_COMP(int_data, threshold_density)

  }

  if(int_type=="unused"){

    db_inter<-NULL
    warning("The object is not provided because the combination of arguments is not commonly used in plant-plant interaction networks")
  }


  return(db_inter)
}

