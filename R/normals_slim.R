library(sf)
library(terra)
library(tidyverse)
library(multidplyr)
library(magrittr)

###
### All this code is from: https://github.com/mt-climate-office/normal-explorer/blob/main/normal-explorer.Rmd
###


# This is the core of the lmomco::pargam function,
# abstracted out here for speed. All credit goes to
# Hosking, J.R.M., 1996, FORTRAN routines for use with the method of L-moments: Version 3, IBM Research Report RC20525, T.J. Watson Research Center, Yorktown Heights, New York.
# *and*
# Asquith, W.H., 2020, lmomco---L-moments, censored L-moments, trimmed L-moments, L-comoments, and many distributions. R package version 2.3.6, Texas Tech University, Lubbock, Texas.
pargam_slim <-
  function(LL) {
    if (any(is.na(LL[1:2]))) {
      return(c(
        alpha = NA,
        beta = NA
      ))
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
      TT <- pi * LCV^2
      ALPHA <- (1 + A1 * TT) / (TT * (1 + TT * (A2 + TT *
                                                  A3)))
    }
    return(c(
      alpha = ALPHA,
      beta = ALPHA / L1
    ))
  }

# This is a vectorized form of seq.Date
seq_vec <- Vectorize(seq.Date, vectorize.args = c("from"), SIMPLIFY = FALSE)


# Gamma distributions are strictly positive
replace_if_zero <-
  function(x) {
    x[x <= 0] <- 0.01
    x
  }
# This uses a parallelized C++ function from the 'Lmoments' package
# to calculate the first two L-Moments of the distribution
calc_lmoments <-
  function(x) {
    out <- matrix(
      nrow = nrow(x),
      ncol = 2
    )
    not_na <- which(complete.cases(x))
    out[not_na, ] <- Lmoments:::Lmoments_calc(t(x[not_na, ]), rmax = 2L)
    out
  }
# Given a raster stack of precipitation values, this function
# replaces zeros with 0.01mm, calculates the first two L-moments,
# and estimate the alpha and beta parameters of a two-parameter gamma
# distribution.
# It returns a raster with 'alpha' and 'beta' layers
calc_gamma_from_rast <-
  function(x) {
    x[] %>%
      replace_if_zero() %>%
      calc_lmoments() %>%
      apply(1, pargam_slim) %>%
      t() %>%
      terra::setValues(terra::rast(x, nlyrs = 2), .) %>%
      magrittr::set_names(c("alpha", "beta"))
  }

calc_and_write <- 
  function(x, dir_name, period){
    if(! dir.exists(dir_name)) {
      dir.create(dir_name, 
                 recursive = TRUE, 
                 showWarnings = FALSE)
    }
      
    gamma <- 
      calc_gamma_from_rast(x) %T>%
      {
        list(alpha = .$alpha,
             beta = .$beta) %>%
          purrr::imap(~terra::writeRaster(x = .x, 
                                          filename = paste0(dir_name,"/",period, "_", .y, ".tif"),
                                          overwrite = TRUE, 
                                          gdal = c("COMPRESS=DEFLATE", 
                                                   "of=COG"),
                                          # datatype='INT2U',
                                          memfrac = 0.9))
      }
    
    x <- list(
      mode = (gamma$alpha - 1)/gamma$beta,
      median = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.5, shape = alpha, rate = beta)}, usenames = TRUE),
      mean = gamma$alpha/gamma$beta,
      variance = gamma$alpha/((gamma$beta)^2)
    ) %>%
      purrr::imap(~magrittr::set_names(.x, .y)) %>%
      purrr::iwalk(~terra::writeRaster(x = .x, 
                                       filename = paste0(dir_name,"/",period, "_", .y, ".tif"),
                                       overwrite = TRUE, 
                                       gdal = c("COMPRESS=DEFLATE", 
                                                "of=COG"),
                                       # datatype='INT4S',
                                       memfrac = 0.9))
    
    q <- list(
      '1th' = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.01, shape = alpha, rate = beta)}, usenames = TRUE),
      '10th' = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.1, shape = alpha, rate = beta)}, usenames = TRUE),
      '20th' = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.2, shape = alpha, rate = beta)}, usenames = TRUE),
      '30th' = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.3, shape = alpha, rate = beta)}, usenames = TRUE),
      '40th' = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.4, shape = alpha, rate = beta)}, usenames = TRUE),
      '50th' = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.5, shape = alpha, rate = beta)}, usenames = TRUE),
      '60th' = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.6, shape = alpha, rate = beta)}, usenames = TRUE),
      '70th' = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.7, shape = alpha, rate = beta)}, usenames = TRUE),
      '80th' = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.8, shape = alpha, rate = beta)}, usenames = TRUE),
      '90th' = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.9, shape = alpha, rate = beta)}, usenames = TRUE),
      '99th' = terra::lapp(gamma, fun = function(alpha, beta){qgamma(0.99, shape = alpha, rate = beta)}, usenames = TRUE)
      ) %>%
      purrr::imap(~magrittr::set_names(.x, .y)) %>%
      purrr::iwalk(~terra::writeRaster(x = .x, 
                                       filename = paste0(dir_name,"/quantiles/",period, "_", .y, ".tif"),
                                       overwrite = TRUE, 
                                       gdal = c("COMPRESS=DEFLATE", 
                                                "of=COG"),
                                       # datatype='INT4S',
                                       memfrac = 0.9))
    
    
    return(dir_name)
  }

iterate_options <- function(v, period, out_dir, mask = NA) {

  out_dir <- file.path(out_dir, v)
  r <- list.files(
    glue::glue("~/data/gridmet/processed/montana/{v}/summarized/{period}"),
    full.names = T
  ) %>%
    grep(paste(1991:2020, collapse="|"), ., value = T) %>% 
    terra::rast()
  
  if (!is.na(mask)) {
    r <- terra::mask(r, mask)
  }

  if (period == "monthly") {
    purrr::map(1:12, function(x) {
      s <- terra::subset(r, which(rep(1:12, 30) == x))
      calc_and_write(x=s, dir_name=out_dir, period=tolower(month.abb[x]))
    })
  }  else {
    calc_and_write(x=r, dir_name=out_dir, period='annual')
  }
  
  return("yay")
}

mask = mcor::mt_state_simple %>% sf::st_transform(4326) %>% terra::vect()

out_dir = "~/data/gridmet/processed/montana/normals"
tidyr::crossing(
  variables = c("erc", "rmax", "rmin", "sph", "srad", "vpd", "vs", "tmmx", "tmmn", "pr"),
  periods = c("monthly", "annual")
) %>%
  dplyr::rowwise() %>% 
  dplyr::mutate(complete = iterate_options(variables, periods, out_dir, mask))


