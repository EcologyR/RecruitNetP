#' Recruitment niche: community-level effects on species recruitment
#'
#' @description
#' tests whether the presence of vegetation (i.e any canopy species) compared to the
#' "Open", enhances, depresses or has a neutral effect on the recruitment of a given
#' recruit species. **Input**:The canopy-recruit interactions dataset and the canopy
#' cover dataset (details as explained in [int_significance()] documentation).
#'
#' @inheritParams check_interactions
#' @inheritParams check_cover
#'
#' @returns The same variables provided by the function [int_significance()]
#' but without distinguishing between canopy species.

#'
#' @export
#'
#' @examples
#' recruitment_niche_test (Amoladeras_int, Amoladeras_cover)
#'
recruitment_niche_test <- function(int_data,cover_data){

  cover_data$Ac <- (cover_data$Cover / 100) * cover_data$Sampled_distance_or_area
  Av <- sum(cover_data[cover_data$Canopy!= "Open",]$Ac)
  Ao <- sum(cover_data[cover_data$Canopy== "Open",]$Ac)
  sp_Fr <- stats::aggregate(Frequency ~ Recruit, data = int_data [int_data$Canopy!="Open",], sum)
  sp_Fro <- stats::aggregate(Frequency ~ Recruit, data = int_data [int_data$Canopy=="Open",], sum)
  df<-merge (sp_Fr, sp_Fro, by="Recruit", all.x=T, all.y=T)
  colnames(df) <- c("Recruit", "sp_Fr", "sp_Fro")
  df$sp_Av<-rep(Av, nrow(df))
  df$sp_Ao<-rep(Ao, nrow(df))
  df$sp_Fr[is.na(df$sp_Fr)] <-0
  df$sp_Fro[is.na(df$sp_Fro)] <-0
  n_tests <- dim(df)[1]
  df<- df [,c("Recruit", "sp_Fr", "sp_Av", "sp_Fro", "sp_Ao")]
  colnames(df) <- c("Recruit", "Fr", "Av", "Fro", "Ao")
  df$exp_p <- df$Av/(df$Av+df$Ao) # Expected probability of success (i.e. of recruiting
  # under canopy)

  # Testability through Binomial test

  df$Ftot <- df$Fr+df$Fro

  extreme_p <- c()
  for(i in 1:n_tests){
    extreme_p[i] <- min(df$exp_p[i], 1-df$exp_p[i])
  }
  df$extreme_p <- extreme_p

  testability <- c()
  for(i in 1:n_tests) {
    testability[i] <- stats::binom.test(df$Ftot[i], df$Ftot[i], df$extreme_p[i],
                                 alternative ="two.sided")$p.value
  }
  df$testability <- testability

  # Binomial (or Chi square) Test Significance

  Significance <- c()
  for(i in 1:n_tests) {
    ifelse(((df$Fr[i]+df$Fro[i])*(df$Av[i]/(df$Av[i]+df$Ao[i]))<=5 | (df$Fr[i]+df$Fro[i])*(df$Ao[i]/(df$Av[i]+df$Ao[i]))<=5),
           Significance[i] <- stats::binom.test(df$Fr[i], df$Fr[i]+df$Fro[i], df$exp_p[i], alternative ="two.sided")$p.value,
           Significance[i] <- stats::chisq.test(c(df$Fr[i], df$Fro[i]), p = c(df$exp_p[i], 1-df$exp_p[i]))$p.value
    )
  }
  df$Significance <- Significance

  Test_type <- c()
  for(i in 1:n_tests) {
    ifelse(((df$Fr[i]+df$Fro[i])*(df$Av[i]/(df$Av[i]+df$Ao[i]))<=5 | (df$Fr[i]+df$Fro[i])*(df$Ao[i]/(df$Av[i]+df$Ao[i]))<=5),
           Test_type[i] <- "Binomial",
           Test_type[i] <- "Chi-square"
    )
  }
  df$Test_type <- Test_type

  message("P-values are unadjusted for multiple tests")
  if(length(unique(df$Test_type))>1) warning("Different tests were used for different canopy-recruit pairs. Check column Test_type")

  Effect_int <- c()
  for(i in 1:n_tests) {
    ifelse((df$testability[i]>0.05),
           Effect_int[i] <- "Not testable",
           ifelse(df$Significance[i] > 0.05,
                  Effect_int[i] <- "Neutral",
                  ifelse((df$Fr[i]/df$Av[i])>(df$Fro[i]/df$Ao[i]),
                         Effect_int[i] <- "Facilitated",
                         Effect_int[i] <- "Depressed")
           )
    )
  }

  df$Veg_effect <- Effect_int
  drops <- c("exp_p", "Ftot", "extreme_p")
  df <- df[ , !(names(df) %in% drops)]
  return(df)
}


