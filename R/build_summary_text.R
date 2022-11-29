historical_temp_change <- function(r, shp) {
  shp %<>% dplyr::mutate(name = "domain")

  change <- r %>%
    magrittr::set_names(lubridate::year(terra::time(.))) %>%
    terra::app(function(ys) {
      if(all(is.na(ys))) {
        return(NA)
      }
      xs <- as.numeric(names(ys))
      mod <- lm(ys ~ xs)
      coeffs <- coef(mod)

      meta <- summary(mod)
      if (meta$coefficients[,4][2] > 0.05) {
        return(0)
      }
      coeffs[2] * length(ys)
    })

}
