#' @param x a numeric vector or matrix. `x` and `y` can also both be factors.
#' @param y a numeric vector; ignored if `x` is a matrix. `x` and `y` can also both be factors.
#' @param correct a logical indicating whether to apply continuity correction in the case of a 2x2 table. Default is TRUE.
#' @param p a vector of probabilities of the same length as `x`, used for goodness-of-fit tests. Default is rep(1/length(x), length(x)).
#' @param rescale.p a logical indicating whether to rescale `p` to sum to 1. Default is FALSE.
#' @param simulate.p.value a logical indicating whether to compute p-values by Monte Carlo simulation. Default is FALSE.
#' @param B an integer specifying the number of replicates used in the Monte Carlo test. Default is 2000.
#'
#' @return A list with class "htest".
#' @noRd
#' @details
#' The original function chisq.test performs a chi-squared test of the null hypothesis that the
#' proportions in a one-way table (goodness-of-fit) or the counts in a two-way
#' table (contingency) follow a specified distribution.
#' chisq.test2 is a modified version of chisq.test function that optimices the use of its argument simulated.p.value =T
#' This version uses tabulate instead of table to create frequency counts
#'
#' @examples
#' ## Goodness-of-fit test
#' chisq.test(c(4, 6, 17, 16, 8, 9), p = c(1, 1, 1, 1, 1, 1)/6)


chisq.test2 <-function (x, y = NULL, correct = TRUE, p = rep(1/length(x), length(x)),
                        rescale.p = FALSE, simulate.p.value = FALSE, B = 2000)
{
  DNAME <- deparse(substitute(x))
  if (is.data.frame(x))
    x <- as.matrix(x)
  if (is.matrix(x)) {
    if (min(dim(x)) == 1L)
      x <- as.vector(x)
  }
  if (!is.matrix(x) && !is.null(y)) {
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length")
    DNAME2 <- deparse(substitute(y))
    xname <- if (length(DNAME) > 1L || nchar(DNAME, "w") >
                 30)
      ""
    else DNAME
    yname <- if (length(DNAME2) > 1L || nchar(DNAME2, "w") >
                 30)
      ""
    else DNAME2
    OK <- stats::complete.cases(x, y)
    x <- factor(x[OK])
    y <- factor(y[OK])
    if ((nlevels(x) < 2L) || (nlevels(y) < 2L))
      stop("'x' and 'y' must have at least 2 levels")
    x <- table(x, y)
    names(dimnames(x)) <- c(xname, yname)
    DNAME <- paste(paste(DNAME, collapse = "\n"), "and",
                   paste(DNAME2, collapse = "\n"))
  }
  if (any(x < 0) || anyNA(x))
    stop("all entries of 'x' must be nonnegative and finite")
  if ((n <- sum(x)) == 0)
    stop("at least one entry of 'x' must be positive")
  if (simulate.p.value) {
    setMETH <- function() METHOD <<- paste(METHOD, "with simulated p-value\n\t (based on",
                                           B, "replicates)")
    almost.1 <- 1 - 64 * .Machine$double.eps
  }
  if (is.matrix(x)) {
    METHOD <- "Pearson's Chi-squared test"
    nr <- as.integer(nrow(x))
    nc <- as.integer(ncol(x))
    if (is.na(nr) || is.na(nc) || is.na(nr * nc))
      stop("invalid nrow(x) or ncol(x)", domain = NA)
    sr <- rowSums(x)
    sc <- colSums(x)
    E <- outer(sr, sc)/n
    v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
    V <- outer(sr, sc, v, n)
    dimnames(E) <- dimnames(x)
    if (simulate.p.value && all(sr > 0) && all(sc > 0)) {
      setMETH()
      tmp <- .Call(C_chisq_sim, sr, sc, B, E)
      STATISTIC <- sum(sort((x - E)^2/E, decreasing = TRUE))
      PARAMETER <- NA
      PVAL <- (1 + sum(tmp >= almost.1 * STATISTIC))/(B +
                                                        1)
    }
    else {
      if (simulate.p.value)
        warning("cannot compute simulated p-value with zero marginals")
      if (correct && nrow(x) == 2L && ncol(x) == 2L) {
        YATES <- min(0.5, abs(x - E))
        if (YATES > 0)
          METHOD <- paste(METHOD, "with Yates' continuity correction")
      }
      else YATES <- 0
      STATISTIC <- sum((abs(x - E) - YATES)^2/E)
      PARAMETER <- (nr - 1L) * (nc - 1L)
      PVAL <- stats::pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    }
  }
  else {
    if (length(dim(x)) > 2L)
      stop("invalid 'x'")
    if (length(x) == 1L)
      stop("'x' must at least have 2 elements")
    if (length(x) != length(p))
      stop("'x' and 'p' must have the same number of elements")
    if (any(p < 0))
      stop("probabilities must be non-negative.")
    if (abs(sum(p) - 1) > sqrt(.Machine$double.eps)) {
      if (rescale.p)
        p <- p/sum(p)
      else stop("probabilities must sum to 1.")
    }
    METHOD <- "Chi-squared test for given probabilities"
    E <- n * p
    V <- n * p * (1 - p)
    STATISTIC <- sum((x - E)^2/E)
    names(E) <- names(x)
    #     if (simulate.p.value) {
    #         setMETH()
    #        nx <- length(x)
    #         sm <- matrix(sample.int(nx, B * n, TRUE, prob = p),
    #             nrow = n)
    #         ss <- apply(sm, 2L, function(x, E, k) {
    #             sum((table(factor(x, levels = 1L:k)) - E)^2/E)
    #         }, E = E, k = nx)
    #         PARAMETER <- NA
    #         PVAL <- (1 + sum(ss >= almost.1 * STATISTIC))/(B +
    #             1)
    #      }

    if (simulate.p.value) {
      setMETH()
      nx <- length(x)
      sm <- matrix(sample.int(nx, B * n, TRUE, prob = p), nrow = n)
      ss <- apply(sm, 2L, function(col) {
        freq_table <- tabulate(col, nbins = nx)
        sum((freq_table - E)^2 / E)
      })
      PARAMETER <- NA
      PVAL <- (1 + sum(ss >= almost.1 * STATISTIC)) / (B + 1)
    }

    else {
      PARAMETER <- length(x) - 1
      PVAL <- stats::pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    }
  }
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  if (any(E < 5) && is.finite(PARAMETER))
    warning("Chi-squared approximation may be incorrect")
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME, observed = x,
                 expected = E, residuals = (x - E)/sqrt(E), stdres = (x -
                                                                        E)/sqrt(V)), class = "htest")
}
