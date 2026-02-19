#' Title
#'
#' @param int_data
#' @param cover_data
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
#'
visu_funtopol_rec <- function(int_data,cover_data){

  int_data0<-int_data
  cover_data0<-cover_data

  int_data<-comm_to_RN(int_data,cover_data,expand="yes", rm_sp_no_cover="allsp" )

  if (!"Open" %in% int_data$Canopy) stop("ERROR: your data does not contain a node named Open or it is spelled differently.")


  nodes_list <- funtopol_UNI(int_data)$Functional_classification

  int_data_plot<-int_significance(int_data0,cover_data0, int_type=c("rec"))
  int_data_plot <- int_data_plot[, c("Canopy", "Recruit", setdiff(names(int_data_plot), c("Canopy","Recruit")))]

  g <- igraph::graph_from_data_frame(int_data_plot, directed = TRUE)
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE)
  SCCs <- igraph::components(g, mode = "strong")


  core<-funtopol_UNI(int_data)$Descriptors[which(rownames(funtopol_UNI(int_data)$Descriptors)=="Num. core species"),]
  if(core>0){

    open_df <- c("Open", "Open")
    nodes_list <- rbind(nodes_list, open_df)
    nodes_list$label <- nodes_list$id
    int_data <- int_data[which(int_data$Fcr!=0), c("Canopy", "Recruit")]
    g <- igraph::graph_from_data_frame(int_data, directed = TRUE)
    g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = FALSE)
    g<- igraph::as_data_frame(g, what = "both")
    edges_list <- g$edges

    # nodes data.frame for legend
    lnodes <- data.frame(label = c("Open", "Core", "Satellite", "Strict transient", "Disturbance-dependent transient"),
                         shape = c( "dot"), color = c("#F0E442", "#009E73", "#0072B2", "#D55E00", "#CC79A7"),
                         title = "Functional types", id = 1:5)

    # Network visualization and export to html

    network <- visNetwork(nodes_list, edges_list) %>%
      visNetwork::visIgraphLayout(layout = "layout_with_fr") %>%
      visEdges(arrows ="to") %>%
      visGroups(groupname = "Open", color = "#F0E442") %>%
      visGroups(groupname = "Core", color = "#009E73") %>%
      visGroups(groupname = "Satellite", color = "#0072B2") %>%
      visGroups(groupname = "Strict_transients", color = "#D55E00") %>%
      visGroups(groupname = "Disturbance_dependent_transients", color = "#CC79A7") %>%
      visOptions(nodesIdSelection = TRUE) %>%
      visNetwork::visLegend(addNodes = lnodes, useGroups = FALSE)


    htmlwidgets::saveWidget(network, file = "network_recruitment.html", selfcontained = FALSE)# Save the html version of the network

  }else{

    stop("ERROR: This network does not have a Core, and thus the functional topology can not be visualized")}


  return(network)
}

