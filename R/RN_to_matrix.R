#' Title
#'
#' @param int_data
#' @param cover_data
#' @param int_type
#' @param weight
#'
#' @returns
#' @export
#'
#' @examples
#' #This function returns a matrix with canopy species as columns and recruits as rows, containing the association index specified in the weight argument. If int_type is set to "rec," all possible interactions between species with known coverage are displayed. If set to "fac" (or "comp"), a nonzero value appears when recruits associate with canopy plants more (or less) than expected based on their coverage; otherwise, the value is zero.
#'
RN_to_matrix<- function(int_data,cover_data, int_type=c("rec", "fac","comp"), weight = c("Fcr","Dcr","Dro","Ns", "NintC", "NintA", "RII")){

  if (!"Open" %in% int_data$Canopy) stop("ERROR: your data does not contain a node named Open or it is spelled differently.")

  if(int_type=="rec"){

    index<-suppressWarnings(associndex(int_data,cover_data,expand="yes",rm_sp_no_cover="allsp"))
    data<-comm_to_RN_UNI(int_data,cover_data)
    data$Dcr<-data$Fcr/data$Ac

    # calculate Dro for each recruit species
    open_rows <- data[data$Canopy == "Open", ]
    ratios <- tapply(open_rows$Fcr / open_rows$Ac, open_rows$Recruit, mean)
    data$Dro <- ratios[data$Recruit]


    data$int<-paste(data$Canopy,data$Recruit, sep="_")
    index$int<-paste(index$Canopy,index$Recruit, sep="_")
    db<-merge(data[,c("int","Canopy","Recruit","Fcr","Icr","Pcr","Dcr","Dro")],index[,c("int","Ns", "NintC", "NintA", "RII")],   by="int", all.x=T)
    db[is.na(db)]<-0
    net<-suppressWarnings(RN_to_matrix_UNI(db, weight))

    if (weight %in% c("Ns", "NintC", "NintA", "RII")) {
      warning("Since the index specified in the 'weight' argument is defined relative to 'Open', rows or columns labeled 'Open' are mathematically zero and not biologically meaningful")
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
      db<-suppressWarnings(associndex_UNISITE_BI(comm_to_RN_BI(int_data,cover_data)))
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
      db<-suppressWarnings(associndex_UNISITE_BI_COMP(comm_to_RN_UNI_COMP(int_data,cover_data)))
      db$int<-paste(db$Canopy,db$Recruit, sep="_")
      db<-merge(db, data[,c("int","Icr","Pcr")], by="int", all.x=T)
      db<-db[db$int%in%comp_int,]
      net<-suppressWarnings(RN_to_matrix_BI(db, weight))
    }
  }


  return(net)
}

