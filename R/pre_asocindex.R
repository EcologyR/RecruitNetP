#' TODO: title
#'
#' Calculate in separate columns the number of recuits of each species under
#' each species of canopy and in the open and its respective percentage of
#' cover (either for multiple or a single site)
#'
#' @param inter_data data frame with at least four columns:
#' Study_site (unique name of the study site),
#' Recruit (species of the recruit),
#' Canopy (species of the canopy, or "Open"), and
#' Frequency (number of recruits of that species observed under that
#' canopy species in any plot, with "Open" representing recruits observed without a canopy species).
#'
#' @param cover_data data frame with at least five columns:
#' Study_site (unique name of the study site),
#' Plot (unique name of each plot within a study site),
#' Canopy (species of the canopy),
#' Cover (percentage of cover of that species in that plot), and
#' Sampled_distance_or_area (total area of that plot or length in the case of transects).
#'
#' @return data frame with one row for each pair-wise interaction
#' (Recruit speices-Canopy species) in each site, and nine columns:
#' inter_ID (a unique identifier for each pair-wise interaction,
#' combining Study_site, Recruit, and Canopy species),
#' Recruit(recruit species), Canopy (canopy speices),
#' Study_site (name of the study site),
#' Canopy_Freq and Open_Freq with the number of recuits observed
#' under that canopy species or in the Open, respectively and
#' Canopy_cover and Open_cover, with the percentage of the total area sampled
#' in the Study_site occupied by that canopy species and Open respectively.
#' Freq_tot is the sum of Canopy_Freq and Open_Freq (total number of recruits
#' of that speices observed in the area sampled in the Study_site).
#'
#' @export
#'
#' @examples
#' pre_index_all<-pre_asocindex(RecruitNet, CanopyCover)

pre_asocindex <- function(inter_data = RecruitNet,
                          cover_data = CanopyCover
                          ) {
  data <- inter_data
  dbcover <- cover_data

  com <- plot_to_com(data, dbcover)

  inter <- com[[1]]
  Canopy_all <- com[[2]]

  adjlist <- list()
  coverlist <- list()
  edgelist <- list()
  net <- NULL
  NorecrOpen <- NULL
  for (z in 1:length(unique(Canopy_all$Study_site)))
  {
    net[z] <- unique(Canopy_all$Study_site)[z]
    mycovs <- Canopy_all[Canopy_all$Study_site == unique(Canopy_all$Study_site)[z], ]
    alledge <- inter[inter$Study_site == unique(Canopy_all$Study_site)[z], ]
    myadj <- data.frame(reshape2::dcast(data = alledge[, c("Recruit", "Canopy", "Freq")], Recruit ~ Canopy, value.var =
                                          "Freq"))
    myadj2 <- as.matrix(myadj[, -1])
    colnames(myadj2) <- colnames(myadj)[-1]
    rownames(myadj2) <- myadj[, 1]
    myadj <- myadj2
    myadj[is.na(myadj)] <- 0

    #select only those canopies from whcih there is recruit under and cover data( ver & adj mat)
    myadj <- myadj[, colnames(myadj) %in% mycovs$Canopy]

    #Add a canopy called Open if there is no recruits in Open in that community
    if (length(colnames(myadj)[which(colnames(myadj) == "Open")]) < 1) {
      NorecrOpen[z] <- unique(Canopy_all$Study_site)[z]
      myadj <- cbind(myadj, rep(0, dim(myadj)[1]))
      colnames(myadj)[dim(myadj)[2]] <- "Open"
    }
    mycovs <- mycovs[mycovs$Canopy %in% colnames(myadj), ]
    #order the matrix
    myadj <- myadj[, c(mycovs$Canopy[-which(mycovs$Canopy == "Open")], "Open")]
    rownames(mycovs) <- mycovs$Canopy
    mycovs <- mycovs[colnames(myadj), ]


    #expand the database to include the interactions zero in the inetarction matris ( i.e. non-observed interactions)

    obs_inter <- data.frame(expand.grid(dimnames(provideDimnames(myadj)))[1:2], as.vector(as.matrix(myadj)))

    colnames(obs_inter)[which(colnames(obs_inter) == "Var1")] <- "Recruit"
    colnames(obs_inter)[which(colnames(obs_inter) == "Var2")] <- "Canopy"
    colnames(obs_inter)[which(colnames(obs_inter) == "as.vector.as.matrix.myadj..")] <- "Freq"

    obs_inter$Study_site <- rep(paste(net[[z]]), dim(obs_inter)[1])
    obs_inter$inter_ID <-
      paste(obs_inter$Study_site,
            obs_inter$Recruit,
            obs_inter$Canopy,
            sep = "_")

    alledge <- obs_inter[, c("inter_ID", "Freq" , "Study_site", "Canopy", "Recruit")]
    #prepare the database output
    openedge <- alledge[alledge$Canopy == "Open", ]
    myedge <- alledge[alledge$Canopy != "Open", ]

    # add Canopy_cover
    myedge <- merge(myedge, mycovs[, c("Canopy", "Canopy_cover")], by = "Canopy")
    #add Open cover
    myedge$Open_cover <- rep(mycovs[mycovs$Canopy == "Open", "Canopy_cover"], dim(myedge)[1])

    #add number or recruits in open per recruit sp

    myedge <- merge(myedge, openedge[, c("Recruit", "Freq")], by = "Recruit", all.x = T)
    colnames(myedge)[which(colnames(myedge) == "Freq.x")] <- "Canopy_Freq"
    colnames(myedge)[which(colnames(myedge) == "Freq.y")] <- "Open_Freq"
    myedge[is.na(myedge$Open_Freq), "Open_Freq"] <- 0

    myedge <- myedge[which(myedge$Canopy_Freq + myedge$Open_Freq > 0), ]

    adjlist[[z]] <- myadj
    coverlist[[z]] <- mycovs
    edgelist[[z]] <- myedge

  }

  db_inter <- data.frame(do.call("rbind", edgelist))

  db_inter$Freq_tot <- db_inter$Canopy_Freq + db_inter$Open_Freq
  db_inter$Frequency <- db_inter$Canopy_Freq
  return(db_inter)

}
