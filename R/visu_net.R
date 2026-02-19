#' Title
#'
#' @param int_data
#' @param cover_data
#' @param int_type
#' @param weight
#' @param mode
#' @param scale_w
#'
#' @returns
#' @export
#'
#' @examples
#'
#' #Nueva funcion para visualizar la red de interacciones
#'
visu_net<-function(int_data,cover_data,int_type=c("rec","fac","comp"), weight = c("Pcr","Fcr","Dcr","Dro","Ns", "NintC", "NintA", "RII"), mode= c("uni","bi"), scale_w=1) {



  int_type <- match.arg(int_type)
  weight <- match.arg(weight)
  mode <- match.arg(mode)

  if(int_type=="rec" && mode=="bi"){
    warning("Here recruitment networks are considered as replacement networks sensu Alcantara et al. 2019. JVS, 30:1239-1249, and thus unipartite by definition. Therefore, the combination of int_type=rec,and mode=uni is not provided")
  }

  if(int_type=="rec" && mode=="uni"){

    mat <- suppressWarnings(t(RN_to_matrix(int_data, cover_data, int_type = "rec", weight = weight)))
    edge_list <- as.data.frame(as.table(mat))
    colnames(edge_list) <- c("from", "to", "weight")
    edge_list <- subset(edge_list, weight > 0)
    RN_igraph <- graph_from_data_frame(edge_list, directed = TRUE)

    if (weight %in% c("Ns", "NintC", "NintA", "RII")) {
      stop("the index specified in the weight argument uses open as a reference, meanwhile recruitment networks  considered it as a node within the network. This creates an inconsistency as open cannot simultaneously function as a node in the network and as a baseline for weighting interactions.")
    }


    scale<-scale_w # a factor used to adjust the magnitude of the weight ( i.e. width of the arrows)
    # We transpose the adjacency matrix so that arrows point from canopy to recruit,
    #this represents which species will replace a the space ocupied by the canopy in the future.
    plot(RN_igraph,
         edge.arrow.size=.3,
         edge.width = E(RN_igraph)$weight*scale,
         vertex.color="chartreuse",
         vertex.size=8,
         vertex.frame.color="darkolivegreen",
         vertex.label.color="black",
         vertex.label.cex=0.8,
         vertex.label.dist=2,
         vertex.label.font = 3,
         edge.curved=0.2,
         #layout=layout_with_kk(RN_igraph),
         layout=layout_in_circle(RN_igraph),
         frame = TRUE)
    title(main="Recruitment Network")
    return(RN_igraph)



  }


  if(int_type=="fac" && mode=="uni"){


    mat <- RN_to_matrix(int_data, cover_data, int_type = "fac", weight = weight)
    edge_list <- as.data.frame(as.table(mat))
    colnames(edge_list) <- c("from", "to", "weight")
    edge_list <- subset(edge_list, weight > 0)

    RN_igraph <- graph_from_data_frame(edge_list, directed = TRUE)

    scale<-scale_w # a factor used to adjust the magnitude of the weight ( i.e. width of the arrows)
    # We transpose the adjacency matrix so that arrows point from canopy to recruit,
    #this represents which species enhances its recuitment under another species or itself.
    plot(RN_igraph,
         edge.arrow.size=.3,
         edge.width = E(RN_igraph)$weight*scale,
         vertex.color="chartreuse",
         vertex.size=8,
         vertex.frame.color="darkolivegreen",
         vertex.label.color="black",
         vertex.label.cex=0.8,
         vertex.label.dist=2,
         vertex.label.font = 3,
         edge.curved=0.2,
         #layout=layout_with_kk(RN_igraph),
         layout=layout_in_circle(RN_igraph),
         frame = TRUE)
    title(main="Unipartite Recruitment Enhancement Network")

    return(RN_igraph)
  }


  if(int_type=="comp" && mode=="uni"){


    mat <- RN_to_matrix(int_data, cover_data, int_type = "comp", weight = weight)
    edge_list <- as.data.frame(as.table(mat))
    colnames(edge_list) <- c("from", "to", "weight")

    if (weight %in% c("Ns", "NintC", "NintA", "RII")) {
      edge_list$weight<-abs(edge_list$weight)
    }
    edge_list <- subset(edge_list, weight > 0)
    RN_igraph <- graph_from_data_frame(edge_list, directed = TRUE)


    scale<-scale_w # a factor used to adjust the magnitude of the weight ( i.e. width of the arrows)
    # We transpose the adjacency matrix so that arrows point from canopy to recruit,
    #this represents which species enhances its recuitment under another species or itself.
    plot(RN_igraph,
         edge.arrow.size=.3,
         edge.width = E(RN_igraph)$weight*scale,
         vertex.color="chartreuse",
         vertex.size=8,
         vertex.frame.color="darkolivegreen",
         vertex.label.color="black",
         vertex.label.cex=0.8,
         vertex.label.dist=2,
         vertex.label.font = 3,
         edge.curved=0.2,
         #layout=layout_with_kk(RN_igraph),
         layout=layout_in_circle(RN_igraph),
         frame = TRUE)
    title(main="Unipartite Recruitment Depression Network")

    return(RN_igraph)

  }


  if(int_type=="fac" && mode=="bi"){


    a <- RN_to_matrix(int_data, cover_data, int_type = "fac", weight = weight)
    scale<-scale_w
    a<-a*scale
    sorted_a <- sortweb(a, sort.order = "dec")

    # Graficar la red
    plotweb(sorted_a,
            srt = 90,
            higher_italic = TRUE,
            lower_italic = TRUE,
            higher_color = "#E69F00",  # Canopy
            lower_color = "#0072B2"   # Recruit
    )


  }


  if(int_type=="comp" && mode=="bi"){


    a <- RN_to_matrix(int_data, cover_data, int_type = "comp", weight = weight)
    scale<-scale_w

    if(weight%in%c("Ns", "NintC", "NintA", "RII")){a<-a*scale*(-1)}else{a<-a*scale}

    sorted_a <- sortweb(a, sort.order = "dec")

    # Graficar la red
    plotweb(sorted_a,
            srt = 90,
            higher_italic = TRUE,
            lower_italic = TRUE,
            higher_color = "#E69F00",  # Canopy
            lower_color = "#0072B2"   # Recruit
    )

  }
}

