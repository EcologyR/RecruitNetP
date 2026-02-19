#' Title
#'
#' @param int_data
#' @param cover_data
#' @param int_type
#' @param weight
#' @param scale_top
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
RN_heatmap <- function(int_data,cover_data,int_type=c("rec","fac","comp"), weight = c("Pcr","Fcr","Dcr","Dro","Ns", "NintC", "NintA", "RII"), scale_top = 1) {

  if(int_type=="rec"){

    index<-suppressWarnings(associndex(int_data,cover_data,expand="yes",rm_sp_no_cover="allsp"))
    data<-comm_to_RN_UNI(int_data,cover_data)
    data$Dcr<-data$Fcr/data$Ac

    # calculate Dro for each recuit species
    open_rows <- data[data$Canopy == "Open", ]
    ratios <- tapply(open_rows$Fcr / open_rows$Ac, open_rows$Recruit, mean)
    data$Dro <- ratios[data$Recruit]


    data$int<-paste(data$Canopy,data$Recruit, sep="_")
    index$int<-paste(index$Canopy,index$Recruit, sep="_")
    db<-merge(data[,c("int","Canopy","Recruit","Fcr","Icr","Pcr","Dcr","Dro")],index[,c("int","Ns", "NintC", "NintA", "RII")],   by="int", all.x=T)
    db[is.na(db)]<-0

    int_data<-db

    if (weight %in% c("Ns", "NintC", "NintA", "RII")) {
      warning("Since the index specified in the 'weight' argument is defined relative to 'Open', rows or columns labeled 'Open' are mathematically zero and not biologically meaningful")}

    # manually set node order
    canopy_order <- unique(int_data$Canopy)
    canopy_order <- canopy_order[!canopy_order %in% c('Open')]
    canopy_order <- c("Open", canopy_order)
    int_data$Canopy2 <- factor(int_data$Canopy, levels = canopy_order)
    recruit_order <- sort(unique(int_data$Canopy), decreasing = TRUE)
    recruit_order <- recruit_order[!recruit_order %in% c('Open')]
    recruit_order <- c(recruit_order, "Open")
    int_data$Recruit2 <- factor(int_data$Recruit, levels = recruit_order)


    # Make weight variable
    int_data$weight_v <- as.numeric(int_data[[weight]])

    # Lowest (non-zero) and highest values of the weighting variable
    highest_W <- max(int_data$weight_v)
    lowest_W <- min(int_data$weight_v[int_data$weight_v>0])

    # Plot the heatmap
    p<-ggplot(int_data, aes(Canopy2, Recruit2, fill= weight_v)) +
      geom_tile(aes(height = 1, alpha = weight_v != 0),
                colour = "gray", linewidth = 0.25) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = "none") +
      scale_x_discrete(position = "top") +
      labs(fill = weight,x = "Canopy", y = "Recruit") +
      theme(panel.grid = element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))

    return(p)

  }



  if(int_type=="fac"){

    data<-comm_to_RN_BI(int_data,cover_data)
    data$int<-paste(data$Canopy,data$Recruit, sep="_")
    df<-suppressWarnings(int_significance_BI (data))
    df<-df[df$Effect_int=="Enhancing",]
    fac_int<-paste(df$Canopy,df$Recruit, sep="_")
    db<-suppressWarnings(associndex_UNISITE_BI(comm_to_RN_BI(int_data,cover_data)))
    db$int<-paste(db$Canopy,db$Recruit, sep="_")
    db<-merge(db, data[,c("int","Icr","Pcr")], by="int", all.x=T)
    db<-db[db$int%in%fac_int,]

    int_data<-db

    # manually set node order
    canopy_order <- unique(int_data$Canopy)
    int_data$Canopy2 <- factor(int_data$Canopy, levels = canopy_order)
    recruit_order <- sort(unique(int_data$Recruit), decreasing = TRUE)
    int_data$Recruit2 <- factor(int_data$Recruit, levels = recruit_order)

    # Add recruitment density as another weighting variable
    int_data$Dcr <- int_data$Fcr/int_data$Ac

    # Make weight variable
    int_data$weight_v <- as.numeric(int_data[[weight]])

    # Lowest (non-zero) and highest values of the weighting variable
    highest_W <- max(int_data$weight_v)
    lowest_W <- min(int_data$weight_v[int_data$weight_v>0])

    # Plot the heatmap
    p<-ggplot(int_data, aes(Canopy2, Recruit2, fill= weight_v)) +
      geom_tile(aes(height = 1, alpha = weight_v != 0),
                colour = "gray", linewidth = 0.25) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = "none") +
      scale_x_discrete(position = "top") +
      labs(fill = weight,x = "Canopy", y = "Recruit") +
      theme(panel.grid = element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))

    return(p)

  }


  if(int_type=="comp"){


    data<-comm_to_RN_UNI_COMP(int_data,cover_data)
    data$int<-paste(data$Canopy,data$Recruit, sep="_")
    df<-suppressWarnings(int_significance_BI_COMP (data))
    df<-df[df$Effect_int=="Depressing",]
    comp_int<-paste(df$Canopy,df$Recruit, sep="_")
    db<-suppressWarnings(associndex_UNISITE_BI_COMP(comm_to_RN_UNI_COMP(int_data,cover_data)))
    db$int<-paste(db$Canopy,db$Recruit, sep="_")
    db<-merge(db, data[,c("int","Icr","Pcr")], by="int", all.x=T)
    db<-db[db$int%in%comp_int,]

    int_data<-db

    # manually set node order
    canopy_order <- unique(int_data$Canopy)
    int_data$Canopy2 <- factor(int_data$Canopy, levels = canopy_order)
    recruit_order <- sort(unique(int_data$Recruit), decreasing = TRUE)
    int_data$Recruit2 <- factor(int_data$Recruit, levels = recruit_order)

    # Add recruitment density as another weighting variable
    int_data$Dcr <- int_data$Fcr/int_data$Ac

    # Make weight variable
    int_data$weight_v <- as.numeric(int_data[[weight]])

    if(weight%in% c("Fcr","Icr","Pcr","Dcr","Dro")){
      # Lowest (non-zero) and highest values of the weighting variable
      highest_W <- max(int_data$weight_v)
      lowest_W <- min(int_data$weight_v[int_data$weight_v>0])
    }


    if(weight %in% c("Ns", "NintC", "NintA", "RII")){
      nonzero_vals <- int_data$weight_v[int_data$weight_v != 0]
      highest_W <- max(nonzero_vals)
      lowest_W <- min(nonzero_vals)
    }
    # Plot the heatmap
    p<-ggplot(int_data, aes(Canopy2, Recruit2, fill= weight_v)) +
      geom_tile(aes(height = 1, alpha = weight_v != 0),
                colour = "gray", linewidth = 0.25) +
      scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = "none") +
      scale_x_discrete(position = "top") +
      labs(fill = weight,x = "Canopy", y = "Recruit") +
      theme(panel.grid = element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))

    return(p)


  }

}


