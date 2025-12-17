library(magrittr)

dat <- list.files("~/data/cmip/monthly", full.names = T, pattern = ".tif") %>%
  tibble::tibble(f = .) %>%
  dplyr::mutate(
    bn = basename(f) %>%
      tools::file_path_sans_ext()
  ) %>%
  tidyr::separate(bn, c("variable", "model", "scenario"), sep = "_") %>%
  dplyr::filter(variable == "pr")


historical <- dat %>%
  dplyr::filter(scenario == "historical")

append_historical <- function(m, v, r, historical) {

  print(glue::glue("working on {m} for {v}..."))
  tmp <- historical %>%
    dplyr::filter(model == !!m,
                  variable == !!v)

  r1 = terra::rast(tmp$f[[1]])
  r2 = terra::rast(r)

  c(r1, r2)
}

joined <- dat %>%
  dplyr::filter(scenario != "historical") %>%
  dplyr::group_by(model, scenario, variable) %>%
  dplyr::filter(!(model == "MIROC6" && variable == "huss")) %>%
  dplyr::mutate(r =
                  append_historical(
                    dplyr::cur_group()$model,
                    dplyr::cur_group()$variable,
                    f,
                    historical
                  ) %>%
                  list()
  )


joined %>%
  dplyr::select(-f) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    r = ifelse(
      variable == "pr",
      list(r * 86400),
      list(r)
    )
  ) %>%
  dplyr::group_by(variable, scenario) %>%
  dplyr::group_split() %>%
  purrr::map(function(x) {
    v = unique(x$variable)
    scenario = unique(x$scenario)
    fun <- ifelse(v %in% c("pr", "eto", "hargreaves", "above90", "con-dry", "con-wet",
                            "dry-days", "freeze-free", "gdd", "wet-days", "et_m16", "pet_m16",
                            "gpp", "afgnpp", "pfgnpp", "shrnpp", "trenpp"), "sum", "mean")

    r <- terra::rast(x$r)

    if (is.numeric(terra::time(r))) {
      terra::time(r) <- terra::time(r) %>%
        zoo::as.yearmon() %>%
        zoo::as.Date(frac = 0)
    }

    r %>%
      terra::tapp(fun = "mean", index = "days") %>%
      terra::tapp(fun = fun, index = "years") %>%
      {
        terra::subset(., which(terra::time(.) %in% 1995:2024)) %>%
          terra::app(fun = "mean") %>%
          terra::rotate() %>%
          normals::write_as_cog(glue::glue("~/data/cmip/agg/{v}_{scenario}_reference.tif"))

        terra::subset(., which(terra::time(.) %in% 2040:2069)) %>%
          terra::app(fun = "mean") %>%
          terra::rotate() %>%
          normals::write_as_cog(glue::glue("~/data/cmip/agg/{v}_{scenario}_mid.tif"))

        terra::subset(., which(terra::time(.) %in% 2970:2099)) %>%
          terra::app(fun = "mean") %>%
          terra::rotate() %>%
          normals::write_as_cog(glue::glue("~/data/cmip/agg/{v}_{scenario}_end.tif"))

      }
  })

list.files("~/data/cmip/agg", full.names = T) %>%
  tibble::tibble(f = .) %>%
  dplyr::mutate(
    bn = basename(f) %>%
      tools::file_path_sans_ext()
  )  %>%
  tidyr::separate(bn, c("variable", "scenario", "time", "diff"), sep = "_") %>%
  dplyr::filter(is.na(diff)) %>%
  dplyr::select(-diff) %>%
  tidyr::pivot_wider(names_from = time, values_from = f) %>%
  tidyr::pivot_longer(-c(variable, scenario, reference), names_to = "time") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    reference = list(terra::rast(reference)),
    value = list(terra::rast(value)),
    diff = list(normals::write_as_cog(
        value - reference,
        glue::glue("~/data/cmip/agg/{variable}_{scenario}_{time}_diff.tif")
      ))
  )

arrow::read_parquet("https://mco-normals.s3.us-east-2.amazonaws.com/zonal/type=county/id=30063/variable=gpp/part-0.parquet") %>%
  dplyr::group_by(year=as.numeric(year), id) %>%
  dplyr::summarise(value = sum(value)/25.4) %>%
  ggplot(aes(x=year, y=value, color=id)) + geom_line() + geom_point()

