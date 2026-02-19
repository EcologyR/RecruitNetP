#' Convert field data to matrix format
#'
#'
#' @description
#' converts the community data collected in the field, where each row represents
#' a single canopy-recruit interaction, into a matrix format, with recruit
#' species as rows and canopy species as columns. For general recruitment
#' networks, the matrix should be square including all species observed at the study site. This means that the same set of species appears in both rows and columns, with "Open" areas treated as an additional category in both the Canopy and Recruit variables. In contrast, for recruitment enhancement (i.e. facilitation) or depression (i.e. competition), the matrix can be non-square. In this case, rows represent the species whose recruitment is enhanced or suppressed, while columns represent the canopy species that influence this recruitment. These two groups may not include the same species.

#' @inheritParams int_significance
#' @param weight Specifies the metric used to represent interaction strength
#' (i.e., the weight) assigned to each pair of species in the matrix. Explanation of its
#' options (more mathematical information in the description of the function
#' [associndex()] :
#'   - *Fcr*: ***frequency of recruitment***, in number of recruits by
#'   canopy-recruit pair.
#'   - *Dcr*: ***density of recruitment***, as number of recruits per unit
#'   area of canopy species cover.
#'   - *Dro*: ***density of recruitment in open interspaces***, as number of
#'   recruits per unit area of open interspaces.
#'   - *Ns*: The ***Normalized Neighbour Suitability index***; suitable for
#'   comparisons of interaction strength between pairs of species within a
#'   local community.
#'   - *NIntA*: The ***Additive symmetry intensity index***.
#'   - *NIntC*: The ***Commutative symmetry intensity index***.
#'   - *RII*: The ***Relative Interaction Index***.

#'
#' @returns
#'A matrix with recruit species in rows and canopy species in columns,
#'with cells describing the measure selected to describe the interaction
#'strength (i.e. weight) between each pair of species.
#'
#'
#' @export
#'
#' @examples
#'
#' RN_to_matrix(Amoladeras_int, Amoladeras_cover, int_type="rec", weight="Dcr")
#'
#'
RN_to_matrix<- function(int_data,cover_data, int_type=c("rec", "fac","comp"),
                        weight = c("Fcr","Dcr","Dro","Ns", "NintC", "NintA",
                                   "RII")){

  if (!"Open" %in% int_data$Canopy)
    stop("ERROR: your data does not contain a node named Open or it is
         spelled differently.")

  if(int_type=="rec"){

    index<-suppressWarnings(associndex(int_data,cover_data,expand="yes",
                                       rm_sp_no_cover="allsp"))
    data<-comm_to_RN_UNI(int_data,cover_data)
    data$Dcr<-data$Fcr/data$Ac

    # calculate Dro for each recruit species
    open_rows <- data[data$Canopy == "Open", ]
    ratios <- tapply(open_rows$Fcr / open_rows$Ac, open_rows$Recruit, mean)
    data$Dro <- ratios[data$Recruit]


    data$int<-paste(data$Canopy,data$Recruit, sep="_")
    index$int<-paste(index$Canopy,index$Recruit, sep="_")
    db<-merge(data[,c("int","Canopy","Recruit","Fcr","Icr","Pcr","Dcr","Dro")],
              index[,c("int","Ns", "NintC", "NintA", "RII")],
              by="int", all.x=T)
    db[is.na(db)]<-0
    net<-suppressWarnings(RN_to_matrix_UNI(db, weight))

    if (weight %in% c("Ns", "NintC", "NintA", "RII")) {
      warning("Since the index specified in the 'weight' argument is defined
              relative to 'Open', rows or columns labeled 'Open' are
              mathematically zero and not biologically meaningful")
    }
  }

  if(int_type=="fac"){

    data<-comm_to_RN_BI(int_data,cover_data)
    data$int<-paste(data$Canopy,data$Recruit, sep="_")
    df<-suppressWarnings(int_significance_BI (data))

    if(nrow(df[df$Effect_int=="Enhancing",])==0){

      stop("There is not any recruitment enhancing interaction")
    }else{

      df<-df[df$Effect_int=="Enhancing",]
      fac_int<-paste(df$Canopy,df$Recruit, sep="_")
      db<-suppressWarnings(associndex_UNISITE_BI(comm_to_RN_BI(int_data,
                                                               cover_data)))
      db$int<-paste(db$Canopy,db$Recruit, sep="_")
      db<-merge(db, data[,c("int","Icr","Pcr")], by="int", all.x=T)
      db<-db[db$int%in%fac_int,]
      net<-suppressWarnings(RN_to_matrix_BI(db, weight))
    }

  }

  if(int_type=="comp"){

    data<-comm_to_RN_UNI_COMP(int_data,cover_data)
    data$int<-paste(data$Canopy,data$Recruit, sep="_")
    df<-suppressWarnings(int_significance_BI_COMP (data))

    if(nrow(df[df$Effect_int=="Depressing",])==0){

      stop("There is not any recruitment depressing interaction")
    }else{

      df<-df[df$Effect_int=="Depressing",]
      comp_int<-paste(df$Canopy,df$Recruit, sep="_")
      db<-suppressWarnings(associndex_UNISITE_BI_COMP(comm_to_RN_UNI_COMP
                                                      (int_data,cover_data)))
      db$int<-paste(db$Canopy,db$Recruit, sep="_")
      db<-merge(db, data[,c("int","Icr","Pcr")], by="int", all.x=T)
      db<-db[db$int%in%comp_int,]
      net<-suppressWarnings(RN_to_matrix_BI(db, weight))
    }
  }


  return(net)
}

