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
#' #como canopy_service_test_UNI pero sin quitar especies de reclutas
#que no tienen cover ( solo las canopy que no tienen cover)
#sustituimos pre_associndex_UNISITE_UNI por
#associndex(int_data,cover_data, expand="yes", rm_sp_no_cover="onlycanopy")
#calcula para cada especie de nodriza, si alberga bajo su copa una mayor (o menor)
# densidad de reclutas (de cualquier especie) que la esperada en funcion del area que ocupa.
#The input data are two files: the field data containing interactions and species cover
#'
canopy_service_test <- function(int_data,cover_data){

  df<-associndex(int_data,cover_data, expand="yes", rm_sp_no_cover="onlycanopy")
  sp_Fc <- stats::aggregate(Fcr ~ Canopy, data = df, FUN = sum)
  sp_Ac <- stats::aggregate(Ac ~ Canopy, data = df, FUN = max)
  sp_Fro <- stats::aggregate(Fro ~ Canopy, data = df, FUN = sum)
  sp_Ao <- stats::aggregate(Ao ~ Canopy, data = df, FUN = max)
  n_tests <- dim(sp_Fc)[1]
  df <- data.frame(c(sp_Fc, sp_Ac, sp_Fro, sp_Ao))
  myvars <- names(df) %in% c("Canopy.1", "Canopy.2", "Canopy.3")
  df <- df[!myvars]
  colnames(df) <- c("Canopy", "Fc", "Ac", "Fro", "Ao")
  df$exp_p <- df$Ac/(df$Ac+df$Ao) # Expected probability of success (i.e. of recruiting under canopy)

  # Testability through Binomial test

  df$Ftot <- df$Fc+df$Fro

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
    ifelse(((df$Fc[i]+df$Fro[i])*(df$Ac[i]/(df$Ac[i]+df$Ao[i]))<=5 | (df$Fc[i]+df$Fro[i])*(df$Ao[i]/(df$Ac[i]+df$Ao[i]))<=5),
           Significance[i] <- binom.test(df$Fc[i], df$Fc[i]+df$Fro[i], df$exp_p[i], alternative ="two.sided")$p.value,
           Significance[i] <- chisq.test(c(df$Fc[i], df$Fro[i]), p = c(df$exp_p[i], 1-df$exp_p[i]))$p.value
    )
  }
  df$Significance <- Significance

  Test_type <- c()
  for(i in 1:n_tests) {
    ifelse(((df$Fc[i]+df$Fro[i])*(df$Ac[i]/(df$Ac[i]+df$Ao[i]))<=5 | (df$Fc[i]+df$Fro[i])*(df$Ao[i]/(df$Ac[i]+df$Ao[i]))<=5),
           Test_type[i] <- "Binomial",
           Test_type[i] <- "Chi-square"
    )
  }
  df$Test_type <- Test_type

  if(length(unique(df$Test_type))>1) message("Different tests were used for different canopy-recruit pairs. Check column Test_type")

  Effect_int <- c()
  for(i in 1:n_tests) {
    ifelse((df$testability[i]>0.05),
           Effect_int[i] <- "Not testable",
           ifelse(df$Significance[i] > 0.05,
                  Effect_int[i] <- "Neutral",
                  ifelse((df$Fc[i]/df$Ac[i])>(df$Fro[i]/df$Ao[i]),
                         Effect_int[i] <- "Facilitative",
                         Effect_int[i] <- "Depressive")
           )
    )
  }

  df$Canopy_effect <- Effect_int
  drops <- c("exp_p", "Ftot", "extreme_p")
  df <- df[ , !(names(df) %in% drops)]
  return(df)
}

