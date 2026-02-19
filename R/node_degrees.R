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
#' #In recruitment networks, node degrees have particular interpretations. The in- and out-degrees of #a node in a recruitment network inform, respectively, about the width of their **recruitment #niche** (number of canopy species that allow their recruitment) and of the **canopy service** they #provide (number of species that recruit in their vicinity). When the degrees are weighted by the #frequency of recruitment, then they can be interpreted, respectively, as the **abundance in the #recruit bank** (number of recruits of a species in the study site) and the **contribution of the #canopy species to the multispecific sapling bank** (number of recruits of any species associated #with the canopy species). To take into account the dominance of certain interactions in the #recruit bank, the weighted degrees can be transformed to the effective number of partners, which #can be interpreted, respectively, as the **effective recruitment niche width** and **effective #canopy service**.

#desde la matriz de interacciones, de reclutamiento, facilitacion o competencia
#para cada fila (recruit species) y columna (canopy) se cuenta la frecuencia (i.e. numero de reclutas) (service)
#luego se hacen las matrices binarias y se cuenta el numero de especies (width)
#'
node_degrees <- function(int_data,cover_data, int_type=c("rec", "fac","comp")) {

  if(int_type=="rec"){

    data<-comm_to_RN_UNI(int_data, cover_data)
    node_deg<-node_degrees_UNI(data)

  }


  if(int_type=="fac"){

    node_deg<-suppressWarnings(node_degrees_BI(int_data,cover_data))

  }

  if(int_type=="comp"){

    node_deg<-suppressWarnings(node_degrees_BI_COMP(int_data,cover_data))

  }

  return(node_deg)
}

