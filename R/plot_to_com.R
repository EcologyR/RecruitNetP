#' Function plot_to_com.It summarize the information of many plots per site into a single data frame at the community level
#'
#' @param dbinter a data frame with frequency of recruits under canopy species species in each plot of many study sites
#' @param dbcover a data frame with the cover of each canopy species per plot in each study site
#'
#' @return a list with three elements: a data frame with the number of recruits observed under each canopy species(including the open), a data frame with the relative cover of each canopy species and Open in each study site and a list of quantitative community matrices (per study site) with recruit and canopy species in rows and columns respectively
#' @export
#' @import dplyr
#'
#' @examples
#'
#'
#




plot_to_com <- function(dbinter, dbcover) {

  options(dplyr.summarise.inform = FALSE)

  dbcover2 <- dbcover[!is.na(dbcover$Cover), ]
  rmnets <- setdiff(dbcover$Study_site, dbcover2$Study_site)

  #add a warning to the user if there is some study_site that does not
  #have information about the canopy of any of its species. Those study sites will be removed

  if (!length(rmnets)==0)
    warning("For some Study_site there is no information about the canopy of any species")


  dbcover<-dbcover[dbcover$Study_site%in%setdiff(dbcover$Study_site, rmnets),]


  dbinter <- dbinter[dbinter$Study_site %in% setdiff(dbinter$Study_site, rmnets), ]


  #check if the pruning of the study sites, if required, have worked properly i.e. now all the study sites have at least one canopy species
  # with information about its cover (if not stop the process)

  if (!(length(setdiff(dbcover$Study_site, dbinter$Study_site))==0|length(setdiff(dbinter$Study_site, dbcover$Study_site))==0))
    stop("Study_site with complete information in the two arguments x,y does not match")

  ###############
#the absolute amount of species surface cover is calculated from its relative % of cover in each study site
#this is necessary to take into account that some plots might have 0 cm2 of a given canopy, and do not miss
#that amount of surface sampled in that plot of the study site

  dbcover$Canopy_cover_abs<-with (dbcover, (Cover*Sampled_distance_or_area)/100)

  surfN<-data.frame(dbcover|>
                      group_by(Study_site, Canopy)|>
                      summarise(Canopy_cover_abs = sum(Canopy_cover_abs,na.rm=TRUE)))


  site_surf<-data.frame(unique(dbcover[,c("Study_site","Plot","Sampled_distance_or_area")])|>
                          group_by(Study_site)|>
                          summarise(Plot_sup = sum(Sampled_distance_or_area)))



  Canopy_all <- merge(surfN, site_surf, by = "Study_site")
  Canopy_all$Canopy_cover <- with(Canopy_all, (Canopy_cover_abs * 100) / Plot_sup)
  Canopy_all <- Canopy_all[, c("Study_site", "Canopy", "Canopy_cover")]

  Canopy_all[Canopy_all$Canopy_cover == 0, "Canopy_cover"] <- "NA"

  Canopy_all$Canopy_cover <- as.numeric(Canopy_all$Canopy_cover)
  dbinter$mynet_sp <- paste(dbinter$Study_site, dbinter$Canopy, sep = "-")
  Canopy_all$mycover_sp <- paste(Canopy_all$Study_site, Canopy_all$Canopy, sep = "-")

  Canopy_all <- Canopy_all[!is.na(Canopy_all$Canopy_cover), ]


  rmnets_anyNA <- unique(dbinter[dbinter$mynet_sp %in% setdiff(unique(dbinter$mynet_sp), (Canopy_all$mycover_sp)), "Study_site"])

  dbcover <- droplevels(dbcover[dbcover$Study_site %in% setdiff(dbcover$Study_site, rmnets_anyNA), ])
  dbinter <- droplevels(dbinter[dbinter$Study_site %in% setdiff(dbinter$Study_site, rmnets_anyNA), ])
  Canopy_all <- droplevels(Canopy_all[Canopy_all$Study_site %in% setdiff(Canopy_all$Study_site, rmnets_anyNA), ])

  Canopy_all$inter_ID<-paste(Canopy_all$Study_site,Canopy_all$Recruit, Canopy_all$Canopy, sep="_")

  inter <- data.frame(
    dbinter |>
      group_by(Study_site, Recruit, Canopy) |>
      summarise( Freq = sum(Frequency)
      )
  )

  fillOpen <- function (x) {
      if(!"Open" %in% x[["Canopy"]]){

   aux <- data.frame(Canopy="Open", Freq=0)
   return(rbind(x, aux))

  } else {return(x)}
  }

 inter <- inter |>
    group_by(Study_site, Recruit) |>
    tidyr::nest() |>
    dplyr::mutate(data = lapply(data, fillOpen)) |>
    tidyr::unnest(cols=c(data))

 inter$inter_ID<-paste(inter$Study_site,inter$Recruit, inter$Canopy, sep="_")
inter<-data.frame(inter)

  Canopy_all <- Canopy_all |> arrange(Study_site)
  inter <- inter |> arrange(Study_site)

#generate teh adjacency matrix of recruit by canopy species
  adjlist <- list()
  for (z in 1:length(unique(Canopy_all$Study_site)))
  {

  myadj <- data.frame(inter[inter$Study_site == unique(Canopy_all$Study_site)[z],c("Study_site", "inter_ID", "Canopy", "Recruit","Freq") ])

  myadj <- maditr::dcast(dbinter = myadj[, c("Recruit", "Canopy", "Freq")], Recruit ~ Canopy, value.var =
                           "Freq", fill = 0)
  nam <- myadj[, 1]
  myadj <- as.matrix(myadj[, -1], rownames = nam$Recruit)

   #selecciono solo aquellas nodrizas de las que tenemos en ambas ( coberturas y adj mat)
  mycovs <- Canopy_all[Canopy_all$Study_site == unique(Canopy_all$Study_site)[z], ]
  myadj <- myadj[, colnames(myadj) %in% mycovs$Canopy]

  mycovs <- mycovs[mycovs$Canopy %in% colnames(myadj), ]
  #y ahora las ordeno
  myadj <- myadj[, c(mycovs$Canopy[-which(mycovs$Canopy == "Open")], "Open")]
  rownames(mycovs) <- mycovs$Canopy
  mycovs <- mycovs[colnames(myadj), ]

 # myadj<-myadj[,c(which(colnames(myadj)!="Open"),which(colnames(myadj)=="Open"))]

  adjlist[[z]]<-myadj
  names(adjlist)[z] <- unique(Canopy_all$Study_site)[z]

  }

  #Hacer test
  identical(unique(Canopy_all$Study_site), unique(inter$Study_site))

  return(list(inter_com=inter, cover_com=Canopy_all, adj_mat = adjlist))

}

