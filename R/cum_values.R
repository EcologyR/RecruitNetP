#' Title
#'
#' @param int_data
#' @param property
#' @param k
#'
#' @returns
#' @export
#'
#' @examples
#'
#' #For study sites sampled in multiple plots, we can use plot accumulation curves to assess whether our estimates of these dimensions are stable. The function cum_values does plot accumulation #curves for parameters that can be obtained with package igraph. For example, for the number of #nodes and links, and connectance we can use, respectively, the igraph functions "vcount", "ecount" and "edge_density". The function may take long time if *k* >> 100 (for speed, we will run this #example with just 20 resamplings).
# cum_values identical to cum_values_UNI
#'
cum_values <- function(int_data, property=c("vcount","ecount","edge_density"), k = 100){
  require(ggplot2)

  if (!"Plot" %in% names(int_data)) stop("ERROR: your interactions data lacks a column named Plots. This function requires data assembled in plots.")
  nPlots <- length(unique(int_data$Plot))

  if (nPlots < 10)
    warning(
      "WARNING: your are using the incidence approach with very few plots."
    )

  if(property=="vcount"){

    part_RNs <- partial_RNs_UNI(int_data, k)
    nSteps <- length(part_RNs)
    borrar <- unlist(part_RNs, recursive = FALSE)
    df <- data.frame(unlist(lapply(borrar, property)))
    colnames(df) <- c("Value")
    df$sampleSize <- sort(rep(c(1:nSteps),k))
    plot_cumm_value <- ggplot(df, aes(x=as.factor(sampleSize), y=Value)) +
      geom_jitter(colour="turquoise3", alpha=0.5, height = 0, width=0.1) +
      geom_point(stat="summary", fun="mean") +
      geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0.3) +
      labs(x="Sample Size (Num. Plots)", y="Value (mean + 95%CI)") +
      ggtitle("Number of species")

    outputs <- list("Data" = df, "Plot" = plot_cumm_value)
    return(outputs)

  }

  if(property=="ecount"){

    part_RNs <- partial_RNs_UNI(int_data, k)
    nSteps <- length(part_RNs)
    borrar <- unlist(part_RNs, recursive = FALSE)
    df <- data.frame(unlist(lapply(borrar, property)))
    colnames(df) <- c("Value")
    df$sampleSize <- sort(rep(c(1:nSteps),k))
    plot_cumm_value <- ggplot(df, aes(x=as.factor(sampleSize), y=Value)) +
      geom_jitter(colour="turquoise3", alpha=0.5, height = 0, width=0.1) +
      geom_point(stat="summary", fun="mean") +
      geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0.3) +
      labs(x="Sample Size (Num. Plots)", y="Value (mean + 95%CI)") +
      ggtitle("Number of interactions")

    outputs <- list("Data" = df, "Plot" = plot_cumm_value)
    return(outputs)

  }

  if(property=="edge_density"){

    part_RNs <- partial_RNs_UNI(int_data, k)
    nSteps <- length(part_RNs)
    borrar <- unlist(part_RNs, recursive = FALSE)
    df <- data.frame(unlist(lapply(borrar, property)))
    colnames(df) <- c("Value")
    df$sampleSize <- sort(rep(c(1:nSteps),k))
    plot_cumm_value <- ggplot(df, aes(x=as.factor(sampleSize), y=Value)) +
      geom_jitter(colour="turquoise3", alpha=0.5, height = 0, width=0.1) +
      geom_point(stat="summary", fun="mean") +
      geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0.3) +
      labs(x="Sample Size (Num. Plots)", y="Value (mean + 95%CI)") +
      ggtitle("Connectance")

    outputs <- list("Data" = df, "Plot" = plot_cumm_value)
    return(outputs)

  }

}

