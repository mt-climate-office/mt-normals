library(magrittr)

dir.create("~/Downloads/tmp")

future::plan(future.callr::callr,
     workers = 5)


list.files("~/Downloads", full.names = T, pattern = ".tif") %>%
  tail(2) %>%
  purrr::map(function(x) {
    print(x)
    r <- terra::rast(x)

    suffix <- x %>%
      stringr::str_split_1("-") %>%
      tail(2) %>%
      paste(collapse = "-")

    dat <- tibble::tibble(n = names(r)) %>%
      tidyr::separate(n, c("month", "v")) %>%
      dplyr::mutate(month = month.abb[as.numeric(month) + 1] %>%
                      tolower(),
                    name = glue::glue("~/Downloads/tmp/{month}_{v}_{suffix}"))

    furrr::future_map2(seq_len(terra::nlyr(r)), dat$name, function(i, name) {
      terra::writeRaster(terra::rast(x)[[i]], name, overwrite = TRUE)
    })
  })

dir.create("~/Downloads/tmp/tmp")
list.files("~/Downloads/tmp", full.names = T, pattern= ".tif") %>%
  tibble::tibble(f = .) %>%
  dplyr::mutate(n = basename(f)) %>%
  tidyr::separate(n, c("mon", "v", "part"), sep= "_") %>%
  dplyr::group_by(mon, v) %>%
  dplyr::group_split() %>%
  furrr::future_map(\(x) {
    print(glue::glue("Making {x$mon[[1]]}_{x$v[[1]]}.tif..."))
    purrr::map(x$f, terra::rast) %>%
      terra::sprc() %>%
      terra::mosaic() %>%
      terra::aggregate(fact=5, fun="mean") %>%
      normals::write_as_cog(glue::glue(
        "~/Downloads/tmp/tmp/{x$mon[[1]]}_{x$v[[1]]}.tif"
      ))
  })


df <- list.files("~/Downloads/tmp/tmp", full.names = T) %>%
  tibble::tibble(f = .) %>%
  dplyr::mutate(n = basename(f) %>% tools::file_path_sans_ext()) %>%
  tidyr::separate(n, c("mon", "v")) %>%
  dplyr::mutate(new = glue::glue("~/data/gee/{tolower(v)}/normals/1995-2024/{tolower(basename(f)) %>% tools::file_path_sans_ext()}_mean.tif"))

dirs <- unique(dirname(df$new))
lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE)
file.rename(df$f, df$new)


list.files("~/data/gee", full.names = T, recursive = T, pattern = ".tif") %>%
  grep("npp", ., value = T) %>%
  grep("annual", ., value = T, invert = T) %>%
  tibble::tibble(f = .) %>%
  dplyr::mutate(n = basename(f) %>% tools::file_path_sans_ext()) %>%
  tidyr::separate(n, c("m", "v", "d")) %>%
  dplyr::group_by(v) %>%
  dplyr::summarise(
    name = dplyr::first(f) %>% dirname() %>%
      paste0(glue::glue("/annual_{dplyr::first(v)}.tif")),
    r = list(terra::rast(f) %>%
                              terra::app(fun = "sum") %>%
              normals::write_as_cog(name)))


#### Landcover ####

library(magrittr)

r <- list.files("~/Downloads/", full.names = T) %>%
  lapply(terra::rast) %>%
  terra::sprc() %>%
  terra::mosaic()

r %>%
  terra::aggregate(fact = 5, fun = "mean") %>%
  normals::write_as_cog(
    glue::glue("~/data/gee/{tolower(names(r))}cov/normals/1995-2024/annual_{tolower(names(r))}cov_mean.tif")
  )



