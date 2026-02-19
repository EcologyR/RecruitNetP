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
#' #como recruitment_niche_test_UNI pero sin quitar especies de reclutas
#que no tienen cover ( solo las canopy que no tienen cover)
#sustituimos pre_associndex_UNISITE_UNI por
#associndex(int_data,cover_data, expand="yes", rm_sp_no_cover="onlycanopy")
#calcula para cada especie de recluta, si se establece con una mayor (o menor)
# densidad de reclutas (bajo cualquier especie de canopy) que la esperada en funcion del area que ocupa las canopies en conjunto y el open.
#The input data are two files: the field data containing interactions and species cover
#'
recruitment_niche_test <- function(int_data,cover_data){

  df <- associndex(int_data,cover_data, expand="yes", rm_sp_no_cover="onlycanopy")
  sp_Fr <- stats::aggregate(Fcr ~ Recruit, data = df, FUN = sum)
  sp_Av <- stats::aggregate(Ac ~ Recruit, data = df, FUN = sum)
  sp_Fro <- stats::aggregate(Fro ~ Recruit, data = df, FUN = max)
  sp_Ao <- stats::aggregate(Ao ~ Recruit, data = df, FUN = max)
  n_tests <- dim(sp_Fr)[1]
  df <- data.frame(c(sp_Fr, sp_Av, sp_Fro, sp_Ao))
  myvars <- names(df) %in% c("Recruit.1", "Recruit.2", "Recruit.3")
  df <- df[!myvars]
  colnames(df) <- c("Recruit", "Fr", "Av", "Fro", "Ao")
  df$exp_p <- df$Av/(df$Av+df$Ao) # Expected probability of success (i.e. of recruiting under canopy)

  # Testability through Binomial test

  df$Ftot <- df$Fr+df$Fro

  extreme_p <- c()
  for(i in 1:n_tests){
    extreme_p[i] <- min(df$exp_p[i], 1-df$exp_p[i])
  }
  df$extreme_p <- extreme_p

  testability <- c()
  for(i in 1:n_tests) {
    testability[i] <- binom.test(df$Ftot[i], df$Ftot[i], df$extreme_p[i], alternative ="two.sided")$p.value
  }
  df$testability <- testability

  # Binomial (or Chi square) Test Significance

  Significance <- c()
  for(i in 1:n_tests) {
    ifelse(((df$Fr[i]+df$Fro[i])*(df$Av[i]/(df$Av[i]+df$Ao[i]))<=5 | (df$Fr[i]+df$Fro[i])*(df$Ao[i]/(df$Av[i]+df$Ao[i]))<=5),
           Significance[i] <- binom.test(df$Fr[i], df$Fr[i]+df$Fro[i], df$exp_p[i], alternative ="two.sided")$p.value,
           Significance[i] <- chisq.test(c(df$Fr[i], df$Fro[i]), p = c(df$exp_p[i], 1-df$exp_p[i]))$p.value
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

