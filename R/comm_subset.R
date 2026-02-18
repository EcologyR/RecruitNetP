#' Subset data from one study site
#'
#' @description
#' this function is to be used with datasets containing multiple study sites
#'
#' @param data a dataframe. Must contain a column named Study_site
#' @param site A character string indicating the name of the study site
#'
#' @returns a dataframe
#' @export
#'
#' @examples
#'
#' data<-load_RN()
#' comm_subset(data, site="Amoladeras")
#'
comm_subset <- function(data=NULL, site = NULL) {
  stopifnot("Study_site" %in% names(data))
  if (!is.null(site) ) stopifnot(length(site)==1)

  if(length(unique(data$Study_site))>1 & is.null(site)) stop("You must enter a site name.")
  # Formatting
  ifelse(is.null(site),
         df <- data,
         df <- data[data$Study_site %in% site, ])
  return(df)
}

