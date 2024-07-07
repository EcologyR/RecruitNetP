#' TODO: name
#'
#' Calculate a significance tests of association for each row.
#' Rows can be a pair-wise interaction (Recruit species-Canopy species interaction),
#' the number of recruits observed under any canopy species (Recruit species)
#' or the number of Recruits of any given speies under a given canopy species (Canopy species)
#'
#' @param data_freq data frame obtained as the output of the functions pre_asocindex,
#' recruit_level,canopy_level, or com_level
#' @param iteration number of iterations
#' @param threshold minimun number of recruits to perform the binomial test
#'
#' @return data frame with the same structure as the input with three additional columns:
#' int_p (p-value of the binomial test of association),
#' int_sign (the sign of the association, being Posive (or Negative) if the
#' association is stronger (or weaker) than expected by the percentage cover of Canopy and Open,
#' and Neutral if there is not enougth power to conduct the test,
#' stdres (standarized resdual quantifying the difference between the observed and expected values),
#' and testability (indicating whether the sample size allow to conduct or not the test
#' being Non-testable those rows in which int_sign = Neutral)
#' @export
#'
#' @examples
#' #sigtest(com_level(RecruitNet, CanopyCover))
#' #sigtest(pre_asocindex(RecruitNet, CanopyCover)) # "very slow"

sigtest <- function(data_freq,
                    iteration = 100,
                    threshold = 5
                    ) {

  db_inter <- data_freq

  db_inter$int_p <- rep(NA, dim(db_inter)[1])
  db_inter$int_sign <- rep(NA, dim(db_inter)[1])

  for (i in 1:dim(db_inter)[1]) {
    if (sum(db_inter$Canopy_Freq[i] + db_inter$Open_Freq[i]) < 100000) {
      test <-
        stats::chisq.test(
          c(db_inter$Canopy_Freq[i], db_inter$Open_Freq[i]),
          p = c(db_inter$Canopy_cover[i], db_inter$Open_cover[i]),
          rescale.p = TRUE,
          simulate.p.value = TRUE
        )
      db_inter$int_p[i] <- test$p.value
      db_inter$stdres[i] <- test$stdres[1]
      db_inter$int_sign[i] <-
        ifelse(
          test$p.value <= 0.05 &
            test$stdres[1] > 0,
          "Positive",
          ifelse(
            test$p.value <= 0.05 & test$stdres[1] < 0,
            "Negative",
            "Neutral"
          )
        )
    }

    if (sum(db_inter$Canopy_Freq[i] + db_inter$Open_Freq[i]) >= 100000) {
      test <-
        stats::chisq.test(
          c(db_inter$Canopy_Freq[i], db_inter$Open_Freq[i]),
          p = c(db_inter$Canopy_cover[i], db_inter$Open_cover[i]),
          rescale.p = TRUE,
          simulate.p.value = FALSE
        )
      db_inter$int_p[i] <- test$p.value
      db_inter$stdres[i] <- test$stdres[1]
      db_inter$int_sign[i] <-
        ifelse(
          test$p.value <= 0.05 &
            test$stdres[1] > 0,
          "Positive",
          ifelse(
            test$p.value <= 0.05 & test$stdres[1] < 0,
            "Negative",
            "Neutral"
          )
        )
    }

  }


  db_inter$testability <- rep("NA", dim(db_inter)[1])

  for (i in 1:dim(db_inter)[1])
  {
    if (db_inter[i, ]$int_sign != "Neutral") {
      db_inter[i, ]$testability <- db_inter[i, ]$int_sign
    }

    if (db_inter[i, ]$int_sign == "Neutral" &
        sum(db_inter[i, ]$Canopy_Freq + db_inter[i, ]$Open_Freq) > 5) {
      db_inter[i, ]$testability <- db_inter[i, ]$int_sign
    }

    if (db_inter[i, ]$int_sign == "Neutral" &
        sum(db_inter[i, ]$Canopy_Freq + db_inter[i, ]$Open_Freq) < 6) {
      n <- sum(db_inter[i, ]$Canopy_Freq + db_inter[i, ]$Open_Freq)
      fcan <-
        ifelse(db_inter[i, ]$Canopy_cover <= db_inter[i, ]$Open_cover, n, 0)
      fopen <-
        ifelse(db_inter[i, ]$Open_cover < db_inter[i, ]$Canopy_cover, n, 0)

      db_inter[i, ]$testability <-
        ifelse(length(which(
          replicate(
            iteration,
            stats::chisq.test(
              c(fcan, fopen),
              p = c(db_inter$Canopy_cover[i], db_inter$Open_cover[i]),
              rescale.p = T,
              simulate.p = T
            )$p.value
          ) < 0.05
        )) > threshold, "testable", "non_testable")
    }

    db_inter[db_inter$testability == "testable", "testability"] <-
      "Neutral"
  }



  return(db_inter)


}
