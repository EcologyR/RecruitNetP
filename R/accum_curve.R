#' Accumulation curve of network dimensions
#'
#' @description
#' Uses plot accumulation curves to assess whether the estimates of the number
#' of nodes, links, and link density (a.k.a. connectance) are stable (see Pulgar
#' et al. 2017 for other descriptors). The function accum_curve plots
#' accumulation curves for parameters that can be obtained with package
#' **`igraph`** (Csardi & Nepusz, 2006).
#'
#' **NOTE**: This function is intended for data sets organised in multiple
#' plots of the same community or locality.
#'
#' @inheritParams check_interactions

#' @param property
#' **property**: indicates the network property, obtained from igraph
#' functions, which accuracy is being evaluated. Only three options are
#' currently available:
#' - *vcount*: accuracy of the estimated number of nodes (using igraph function
#' "vcount").
#' - *ecount*: accuracy of the estimated number of canopy-recruit interactions
#' (using igraph function "ecount").
#' - *edge_density*: accuracy of the estimated network connectance (using
#' igraph function "edge_density).

#' @param k
#' An integer number specifying the number of random repetitions of subsets of
#' *n* plots. In each of the *k* repetitions, a subset of *n* randomly chosen
#' plots is combined to build a partial network for which the indicated
#' property is estimated. High values provide more confident estimates of the
#' accuracy, but the function may take long time if *k* >> 100.
#'
#' @returns
#' The function returns a list of two objects:
#' - A plot representing the mean and 95% Confidence interval (i.e. 1.96 times
#' the standard error) of the estimate of the property selected when an
#' increasing number of randomly selected plots are considered.
#' - A data frame with the cumulative values of the property for each
#' repetition of *k* plots. Provided so you can prepare your own customized
#' accumulation plot.
#'
#' @export
#'
#' @examples
#' accum_links <- accum_curve(Amoladeras_int, property="ecount", k=10)
#' head(accum_links$Data)
#' accum_links$Plot

accum_curve <- function(int_data, property=c("vcount","ecount","edge_density"), k = 100) {

  property <- match.arg(property)

  if (!"Plot" %in% names(int_data)) stop("Your interactions data lacks a column named Plots. This function requires data assembled in plots.")
  nPlots <- length(unique(int_data$Plot))

  if (nPlots < 10)
    warning("You are using the incidence approach with very few plots.")

  prop_settings <- list(
    "vcount" = list(fun = igraph::vcount, title = "Number of species"),
    "ecount" = list(fun = igraph::ecount, title = "Number of interactions"),
    "edge_density" = list(fun = igraph::edge_density, title = "Connectance")
  )

  setting <- prop_settings[[property]]

  part_RNs <- partial_RNs_UNI(int_data, k)
  nSteps <- length(part_RNs)
  partial_networks <- unlist(part_RNs, recursive = FALSE)

  df <- data.frame(Value = unlist(lapply(partial_networks, setting$fun)))
  df$sampleSize <- sort(rep(c(1:nSteps),k))

  plot_cumm_value <- ggplot2::ggplot(df, ggplot2::aes(x=as.factor(sampleSize), y=Value)) +
    ggplot2::geom_jitter(colour="turquoise3", alpha=0.5, height = 0, width=0.1) +
    ggplot2::geom_point(stat="summary", fun="mean") +
    ggplot2::geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0.3) +
    ggplot2::labs(x="Sample Size (Num. Plots)", y="Value (mean + 95%CI)") +
    ggplot2::ggtitle(setting$title)

  outputs <- list("Data" = df, "Plot" = plot_cumm_value)
  return(outputs)

}

