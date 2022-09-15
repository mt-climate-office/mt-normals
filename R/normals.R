### All this code is from: https://github.com/mt-climate-office/normal-explorer/blob/main/normal-explorer.Rmd

# This is the core of the lmomco::pargam function,
# abstracted out here for speed. All credit goes to
# Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments: Version 3, IBM Research Report RC20525, T.J. Watson Research Center, Yorktown Heights, New York.
# *and*
# Asquith, W.H., 2020, lmomco---L-moments, censored L-moments, trimmed L-moments, L-comoments, and many distributions. R package version 2.3.6, Texas Tech University, Lubbock, Texas.
pargam_slim <- function(LL) {
    if (any(is.na(LL[1:2]))) {
      return(c(alpha = NA,
               beta = NA))
    }

    A1 <- -0.308
    A2 <- -0.05812
    A3 <- 0.01765
    B1 <- 0.7213
    B2 <- -0.5947
    B3 <- -2.1817
    B4 <- 1.2113
    L1 <- LL[1]
    LCV <- LL[2] / LL[1]
    if (LCV >= 0.5) {
      TT <- 1 - LCV
      ALPHA <- TT * (B1 + TT * B2) / (1 + TT * (B3 + TT *
                                                  B4))
    } else {
      TT <- pi * LCV ^ 2
      ALPHA <- (1 + A1 * TT) / (TT * (1 + TT * (A2 + TT *
                                                  A3)))
    }
    return(c(alpha = ALPHA,
             beta = ALPHA / L1))
  }


# Gamma distributions are strictly positive
replace_if_zero <- function(x) {
    x[x <= 0] <- 0.001
    x
  }

# This uses a parallelized C++ function from the 'Lmoments' package
# to calculate the first two L-Moments of the distribution
calc_lmoments <- function(x) {
    out <- matrix(nrow = nrow(x),
                  ncol = 2)
    not_na <- which(complete.cases(x))
    out[not_na,] <-
      Lmoments:::Lmoments_calc(t(x[not_na,]), rmax = 2L)
    out
  }

# Given a raster stack of precipitation values, this function
# replaces zeros with 0.01mm, calculates the first two L-moments,
# and estimate the alpha and beta parameters of a two-parameter gamma
# distribution.
# It returns a raster with 'alpha' and 'beta' layers
calc_gamma_from_rast <- function(x) {
    x[] %>%
      replace_if_zero() %>%
      calc_lmoments() %>%
      apply(1, pargam_slim) %>%
      t() %>%
      terra::setValues(terra::rast(x, nlyrs = 2), .) %>%
      magrittr::set_names(c("alpha", "beta"))
  }

# Piecewise function to calculate the mode.
calc_mode <- function(a, b) {
    out = terra::deepcopy(a)
    out[a < 1] <- 0
    out[a >= 1] <- (a - 1) / b

    return(out)
  }

calc_median <- function(alpha, beta) {
    stats::qgamma(0.5, shape = alpha, rate = beta)
  }


#' Calculate gamma normals for an input raster timeseries.
#'
#' @param x A `terra::rast` monthly or annual timeseries that will be used to
#' calculate gamma normals.
#' @param out_dir Optional. The directory to save the results out to.
#' @param descriptor The prefix to use on the filename as an identifier.
#'
#' @importFrom magrittr %>%
#'
#' @return A named list of `terra::rast` summarized to the following items:
#' 'mean', 'median', mode', 'variance', 'alpha', 'beta'.
#' @export
#'
#' @examples
#' a = 1
gamma_from_rast <- function(x, out_dir = NULL, descriptor = "annual", rast_out = FALSE) {

  if (!is.null(out_dir)) {
    if (!dir.exists(out_dir)) {
      dir.create(out_dir,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }
  }

  gamma <- calc_gamma_from_rast(x) %>%
    {
      list(alpha = .$alpha,
           beta = .$beta)
    }

  summarized <- list(
    mode = calc_mode(gamma$alpha, gamma$beta),
    median = terra::lapp(
      terra::rast(gamma),
      fun = calc_median,
      usenames = TRUE
    ),
    mean = gamma$alpha / gamma$beta,
    variance = gamma$alpha / ((gamma$beta) ^ 2)
  ) %>%
    purrr::imap( ~ magrittr::set_names(.x, .y))

  if (!is.null(out_dir)) {
    c(gamma, summarized) %>%
      purrr::iwalk(
        ~ write_as_cog(
          x = .x,
          filename = file.path(out_dir, glue::glue("{descriptor}_{.y}.tif")))
      )
  }

  if (rast_out) {
    return(c(gamma, summarized))
  }
  return(out_dir)
}
