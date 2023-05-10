

#' Association tests of interactions
#'
#' @param db_inter data frame with at leas XX columns called XX
#' @param iteration number of iterations
#' @param threshold minimun number of recruits to perform the binomial test
#'
#' @return data frame with the tests of association
#' @export
#'
#' @examples
intertest <- function(db_inter,
                      iteration = 100,
                      threshold = 5) {
  #check arguments

  stoptifnot("Canopy_Freq" %in% names(db_inter)) #este es un ejemplo

  if (!"Open_Freq" %in% names(db_inter))
    # esta sería otra forma de hacerlo
    stop("names not right")

  #here starts the function

  db_inter$int_p <- rep(NA, dim(db_inter)[1])
  db_inter$int_sign <- rep(NA, dim(db_inter)[1])

  for (i in 1:dim(db_inter)[1]) {
    if (sum(db_inter$Canopy_Freq[i] + db_inter$Open_Freq[i]) < 100000) {
      test <-
        chisq.test(
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
        chisq.test(
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
            chisq.test(
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
