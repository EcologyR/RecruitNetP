#' Visualize a heatmap of the interactions matrix.
#'
#' @inheritParams check_interactions
#' @inheritParams check_cover
#'
#' @param int_type
#' #' Indicates the type of plant-plant interaction that will be presented in the
#' output matrix: general recruitment, recruitment enhancement (i.e.
#' facilitation) or recruitment depression (i.e. competition). It may take three
#' possible values:
#'   - *rec*: Estimates the node degree based on all plant-plant interaction
#'   that contribute to recruitment.
#'   - *fac*: Estimates the node degree based on only those pairwise
#'   interactions that enhance recruitment. Not every observed interaction in
#'   the field has to be present in the matrix. In this case non-detected
#'   interactions are not considered and "Open" is not included as a canopy
#'   species category.
#'   - *comp*: Estimates the node degree based on only those pairwise
#'   interactions that depress recruitment. Not every observed interaction in
#'   the field has to be present in the matrix. However, in this case
#'   non-detected interactions are considered (i.e. expanding with 0 all
#'   possible interactions in the study system). "Open" is not included as a
#'   canopy species category.
#'
#'
#' @param weight
#' Specifies the metric used to represent interaction strength (i.e., the
#' weight) assigned to each pair of species in the matrix. See function
#' **associndex** for further details. The possible options are:
#' - *Fcr*: **frequency of recruitment** in number of recruits by canopy-recruit
#'  pair.
#'  - *Dcr*: **density of recruitment** as number of recruits per unit area of
#'  canopy species.
#'  - *Ns*: The index **Normalized Neighbour Suitability index** (proposed by
#'  Mingo, 2014), suitable for comparisons of interaction strength between pairs
#'  of species within a local community, which should be preferred in general
#'  recruitment networks (Alcantara et al. 2025).
#'  - *NIntA*: The index **additive symmetry intensity index** proposed by
#'  Diaz-Sierra et al. (2017).
#'  - *NIntC*: The index **commutative symmetry intensity index** proposed by
#'  Diaz-Sierra et al. (2017).
#'  - *RII*: The index **Relative Interaction Index** (Armas et al., 2004).
#'
#'
#' @returns
#' Heatmap plot of the weighted network.
#'
#' @export
#'
#' @examples
#' RN_heatmap(Amoladeras_int, Amoladeras_cover, int_type="fac", weight ="Ns")
#'
RN_heatmap <- function(int_data,cover_data,int_type=c("rec","fac","comp"), weight = c("Pcr","Fcr","Dcr","Dro","Ns", "NintC", "NintA", "RII")) {

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
    p<-ggplot2::ggplot(int_data, ggplot2::aes(Canopy2, Recruit2, fill= weight_v)) +
      ggplot2::geom_tile(ggplot2::aes(height = 1, alpha = weight_v != 0),
                colour = "gray", linewidth = 0.25) +
      ggplot2::scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = "none") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::labs(fill = weight,x = "Canopy", y = "Recruit") +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=0))

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
    p<-ggplot2::ggplot(int_data, ggplot2::aes(Canopy2, Recruit2, fill= weight_v)) +
      ggplot2::geom_tile(ggplot2::aes(height = 1, alpha = weight_v != 0),
                colour = "gray", linewidth = 0.25) +
      ggplot2::scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = "none") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::labs(fill = weight,x = "Canopy", y = "Recruit") +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=0))

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
    p<-ggplot2::ggplot(int_data, ggplot2::aes(Canopy2, Recruit2, fill= weight_v)) +
      ggplot2::geom_tile(ggplot2::aes(height = 1, alpha = weight_v != 0),
                colour = "gray", linewidth = 0.25) +
      ggplot2::scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0), guide = "none") +
      ggplot2::scale_x_discrete(position = "top") +
      ggplot2::labs(fill = weight,x = "Canopy", y = "Recruit") +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=0))

    return(p)


  }

}


