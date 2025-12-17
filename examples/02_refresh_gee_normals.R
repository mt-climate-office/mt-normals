library(rgee)
reticulate::use_python("/home/cbrust/git/py-def-env/.venv/bin/python")
ee_Initialize()
reticulate::import("ee")


get_qa_bits = function(img, from, to) {
  size = 1 + to - from
  msk = bitwShiftL(1, size) - 1
  img$rightShift(from)$bitwiseAnd(msk)
}

clean_mod16 <- function(img) {
  qa = img$select("ET_QC")
  good_quality = get_qa_bits(qa, 0, 0)$eq(0)
  no_clouds = get_qa_bits(qa, 3, 4)$eq(0)
  mask = good_quality$And(no_clouds)

  img$updateMask(mask)$
    select(c("ET", "PET"))$
    copyProperties(img, list("system:time_start"))
}

clean_mod17 <- function(img) {
  qa = img$select("Psn_QC")
  good_quality = get_qa_bits(qa, 0, 0)$eq(0)
  no_clouds = get_qa_bits(qa, 3, 4)$eq(0)
  mask = good_quality$And(no_clouds)

  img$updateMask(mask)$
    select("Gpp")$
    copyProperties(img, list("system:time_start"))
}

clean_mod13 <- function(img) {
  qa = img$select("DetailedQA")
  mask = get_qa_bits(qa, 0, 1)$eq(0)

  img$updateMask(mask)$
    select(c("NDVI", "EVI"))$
    copyProperties(img, list("system:time_start"))
}

year_mon_calc <- function(year, coll, func="sum") {

  months <- ee$List$sequence(1, 12)

  mon_calc <- function(m, func) {
    w <- coll$filter(ee$Filter$calendarRange(year, year, 'year'))$
      filter(ee$Filter$calendarRange(m, m, 'month'))

    if (func == "sum") {
      w <- w$sum()
    } else if (func == "mean") {
      w <- w$mean()
    } else {
      stop("func must be either 'sum' or 'mean'")
    }
    return(
      w$set('year', year)$
        set('month', m)$
        set('system:time_start', ee$Date$fromYMD(year, m, 1))
    )
  }
  return(months$map(rgee::ee_utils_pyfunc(function(x) {
    mon_calc(x, func=func)
  })))
}


process_normals <- function(coll, func, filter_start="2001-01-01") {
  years <- ee$List$sequence(1995, 2024)
  year_mons <- ee$ImageCollection$fromImages(
    years$map(rgee::ee_utils_pyfunc(
      function(x) {
        year_mon_calc(x, coll = coll, func = func)
      }
    ))$flatten()
  )

  year_mons <- year_mons$filterDate(filter_start, "2050-01-01")

  months <- ee$List$sequence(1, 12)
  monthly_mean <- function(m, year_mons) {
    w <- year_mons$filter(ee$Filter$eq('month', m))$mean()

    return(w$set('month', m))$set('system:time_start', ee$Date$fromYMD(1, m, 1))
  }

  out <- ee$ImageCollection$fromImages(
    months$map(rgee::ee_utils_pyfunc(function(x) {
      return(monthly_mean(x, year_mons))
    }))$flatten()
  )

  out = out$toBands()
  return(out)
}


shp <- sf::read_sf("https://mco-normals.s3.us-east-2.amazonaws.com/fgb/hucs.fgb") %>%
  sf::st_transform(4326) %>%
  sf_as_ee()

mod16 = process_normals(
  ee$ImageCollection("MODIS/061/MOD16A2GF")$
    map(clean_mod16),
  "sum",
  "2001-01-01"
) %>%
  rgee::ee_as_rast(region = shp$geometry(), via = "drive", scale=500)


mod17 = process_normals(
  ee$ImageCollection("MODIS/061/MOD17A2HGF")$
    map(clean_mod17),
  "sum",
  "2001-01-01"
) %>%
  rgee::ee_as_rast(region = shp$geometry(), via="drive", scale=500)

mod13 = process_normals(
  ee$ImageCollection("MODIS/061/MOD13A1")$
    map(clean_mod13),
  "mean",
  "2001-01-01"
) %>%
  rgee::ee_as_rast(region = shp$geometry(), via="drive", scale=500) %>%
  normals::write_as_cog("./mod13.tif")

cover = ee$ImageCollection("projects/rap-data-365417/assets/vegetation-cover-v3")$filterDate("1995-01-01", "2024-12-31")$mean() %>%
  rgee::ee_as_rast(region = shp$geometry(), via="drive", scale=100)%>%
  normals::write_as_cog("./cover.tif")
npp = process_normals(
    ee$ImageCollection("projects/rap-data-365417/assets/npp-partitioned-16day-v3"),func = "sum",filter_start = "1995-01-01"
  ) %>%
  rgee::ee_as_rast(region = shp$geometry(), via="drive", scale=100) %>%
  normals::write_as_cog("./npp.tif")

process_to_normal_cogs <- function(r, out_dir = "~/data/gee", scale_factor=0.0001) {
  nms <- names(r)

  var_locs <- stringr::str_split(nms, "_") %>%
    purrr::map(magrittr::extract, 2) %>%
    unlist()

  vars <- unique(var_locs)

  vars %>%
    purrr::map(function(x) {

      write_dir = file.path(out_dir, tolower(x))
      if (!dir.exists(write_dir)) {
        dir.create(write_dir)
      }
      out_names <- file.path(write_dir, paste0(tolower(month.abb), "_mean.tif"))
      terra::subset(r, stringr::str_detect(var_locs, paste0("^",x,"$"))) %>%
        magrittr::set_names(tolower(month.abb)) %>%
        {. * scale_factor} %>%
        normals::write_as_cog(out_names)

    })
}

make_rapp_normals <- function(r, out_dir) {
  f_names <- file.path(out_dir, tolower(names(r)))
  purrr::map(f_names, dir.create)
  normals::write_as_cog(r, paste0(f_names, "/annual_mean.tif"))
}

make_modis_annual_normals <- function(data_dir, func="sum") {
  list.files(
    data_dir,
    full.names = T,
    recursive = T
  ) %>%
    stringr::str_subset(
      month.abb %>%
        tolower() %>%
        paste(collapse = "|")
    ) %>%
    tibble::tibble(f = .) %>%
    dplyr::mutate(
      v = basename(dirname(f))
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      func = ifelse(stringr::str_detect(v, "et|pet|gpp"), "sum", "mean"),
    ) %>%
    dplyr::group_by(v, func) %>%
    dplyr::summarise(r = list(terra::rast(f)),
                     dir_path = unique(dirname(f))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(r = list(terra::app(r, func, na.rm = T) %>%
                             magrittr::set_names("mean") %>%
                             normals::write_as_cog(file.path(dir_path, "annual_mean.tif"))))
}


reduce_to_region <- function(shp, r, out_name, resolution=500) {
  print(glue::glue("Working on {out_name}..."))
  reduced <- r$map(function(image) {
    image$reduceRegions(
      collection = shp,
      reducer = ee$Reducer$mean(),
      scale = resolution
    )$map(function(f) {
      f$set(
        list(
          date = image$date()$format()
        )
      )
    })
  })$flatten()$map(
    function(x) {
      ee$Feature(NULL, x$toDictionary())
    }
  )

  dat <- rgee::ee_as_sf(reduced, maxFeatures=75000, via="drive")

  dat %>%
    sf::st_drop_geometry() %>%
    tidyr::pivot_longer(-c(date, id, name), names_to = "variable") %>%
    readr::write_csv(out_name)
}

blm = sf::read_sf("https://mco-normals.s3.us-east-2.amazonaws.com/fgb/blm.fgb")
county = sf::read_sf("https://mco-normals.s3.us-east-2.amazonaws.com/fgb/counties.fgb")
huc = sf::read_sf("https://mco-normals.s3.us-east-2.amazonaws.com/fgb/hucs.fgb")
tribes = sf::read_sf("https://mco-normals.s3.us-east-2.amazonaws.com/fgb/tribes.fgb")

tidyr::crossing(
  product = c("mod16", "mod17", "npp", "mod13", "myd13", "cover"),
  # product = c("mod16"),
  loc_type = c("county")# c("huc", "tribe", "blm")
) %>%
  dplyr::mutate(
    resolution = dplyr::case_when(
      product %in% c("mod13", "mod16", "mod17") ~ 500,
      TRUE ~ 30
    ),
    shp =
      dplyr::case_when(
        loc_type == "county" ~ list(county %>%
                                      rgee::sf_as_ee())
        # loc_type == "huc" ~ list(huc %>%
        #                            rgee::sf_as_ee()),
        # loc_type == "tribe" ~ list(tribes %>%
        #                              rgee::sf_as_ee()),
        # loc_type == "blm" ~ list(blm %>%
        #                            rgee::sf_as_ee())

      ),
    coll =
      dplyr::case_when(
        product == "mod16" ~ list(ee$ImageCollection("MODIS/061/MOD16A2GF")$map(clean_mod16)),
        product == "mod17" ~ list(ee$ImageCollection("MODIS/061/MOD17A2HGF")$map(clean_mod17)),
        product == "mod13" ~ list(ee$ImageCollection("MODIS/061/MOD13A1")$map(clean_mod13)),
        product == "myd13" ~ list(ee$ImageCollection("MODIS/061/MYD13A1")$map(clean_mod13)),
        product == "cover" ~ list(ee$ImageCollection("projects/rap-data-365417/assets/vegetation-cover-v3")),
        product == "npp" ~ list(ee$ImageCollection("projects/rap-data-365417/assets/npp-partitioned-16day-v3"))
      ),
    out_name = glue::glue("./data/ee_extract/{loc_type}_{product}.csv")
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(extracted = list(reduce_to_region(shp, coll, out_name, resolution)))



combine_mod13 <- function(data_dir) {
  list.files(data_dir, full.names = T, pattern = "myd13|mod13") %>%
    tibble::tibble(f = .) %>%
    dplyr::mutate(b = basename(f) %>%
                    tools::file_path_sans_ext()) %>%
    tidyr::separate(b, c("loc", "model")) %>%
    dplyr::group_by(loc) %>%
    dplyr::summarise(
      dat = list(
        purrr::map_df(f, readr::read_csv, show_col_types=FALSE)
      )
    ) %>%
    dplyr::mutate(out = glue::glue("./data/ee_extract/{loc}_mcd13.csv")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dat = list(readr::write_csv(dat, out)))
}


# Specify the profile name from your ~/.aws/config file
Sys.setenv(AWS_PROFILE = "default")

# Create an S3 client using the specified profile
s3 <- paws::s3()
s3$list_buckets()

# List all objects
objects <-
  paws::paginate(
    s3$list_objects_v2(Bucket = "mco-normals", Prefix = "cog/")
  )


# Convert to tidy dataframe
out <- purrr::map(
  objects, function(x){
    tibble::tibble(
      key = purrr::map_chr(x$Contents, "Key")
    )
  }
) %>%
  dplyr::bind_rows() %>%
  # dplyr::filter(stringr::str_detect(key, "\\.tif$")) |>
  # dplyr::filter(stringr::str_detect(key, "normals/")) %>%
  dplyr::mutate(
    dirname = stringr::str_extract(key, "(?<=cog/)[^/]+"),
    filename = basename(key) %>%
      tools::file_path_sans_ext(),
  ) %>%
  dplyr::filter(dirname %in% c("afg", "afgnpp", "bgr", "evi", "gpp", "ltr",
                               "m16_et", "m16_pet", "ndvi", "pfg", "pfgnpp", "shr",
                               "shrnpp", "tre", "trenpp")) %>%
  tidyr::separate(filename, c("time", "variable", "metric"), sep = "_") %>%
  dplyr::mutate(
    variable = dplyr::case_when(
      is.na(metric) ~ dirname,
      .default = variable
    ),
    dirname = dplyr::case_when(
      dirname != variable ~ variable,
      .default = dirname
    ),
    metric = "mean",
    filename = glue::glue("{time}_{variable}_{metric}.tif")
  ) %>%
  dplyr::mutate(new_key = glue::glue("cog/{dirname}/normals/1995-2024/{filename}"))


purrr::walk2(out$key, out$new_key, ~{
  s3$copy_object(
    Bucket = "mco-normals",
    CopySource = paste0("mco-normals/", .x),
    Key = .y
  )
  # s3$delete_object(
  #   Bucket = "mco-normals",
  #   Key = .x
  # )
})



# Specify the profile name from your ~/.aws/config file
Sys.setenv(AWS_PROFILE = "default")

# Create an S3 client using the specified profile
s3 <- paws::s3()
s3$list_buckets()

# List all objects
objects <-
  paws::paginate(
    s3$list_objects_v2(Bucket = "mco-normals", Prefix = "zonal/")
  )


out <- purrr::map(
  objects, function(x){
    tibble::tibble(
      key = purrr::map_chr(x$Contents, "Key")
    )
  }
) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(
    stringr::str_detect(
      key, "afg|bgr|ltr|pfg|shr|tre"
    )
  ) %>%
  dplyr::filter(
    stringr::str_detect(
      key, "npp", negate = TRUE
    )
  ) %>%
  dplyr::mutate(
    new_key = stringr::str_replace(key, "variable=([^/]+)", "variable=\\1cov")
  )


purrr::walk2(out$key, out$new_key, ~{
  s3$copy_object(
    Bucket = "mco-normals",
    CopySource = paste0("mco-normals/", .x),
    Key = .y
  )
})


arrow::read_parquet("~/Downloads/stats.parquet") %>%
  dplyr::mutate(
    variable = dplyr::case_when(
      stringr::str_detect(variable, "npp") ~ variable,
      stringr::str_detect(variable, "afg|bgr|ltr|pfg|shr|tre") ~ paste0(variable, "cov"),
      .default = variable
    )
  ) %>%
  arrow::write_parquet("~/Downloads/stats.parquet")
  dplyr::mutate(
    new_key = stringr::str_replace(key, "variable=([^/]+)", "variable=\\1cov")
  )
  dplyr::filter(
    stringr::str_detect(
      variable, "afg|bgr|ltr|pfg|shr|tre"
    )
  ) %>%
  dplyr::filter(
    stringr::str_detect(
      variable, "npp", negate = TRUE
    )
  )


dat <- list.files("~/Downloads", pattern = "_Timeseries_", full.names = T) %>%
  purrr::map(function(x) {
    readr::read_csv(x) %>%
      dplyr::select(-`system:index`, -`.geo`) %>%
      tidyr::pivot_longer(-id) %>%
      tidyr::separate(name, c("variable", "year", "month")) %>%
      tidyr::separate(id, c("type", "id", "name"), sep = "_") %>%
      dplyr::mutate(
        date = lubridate::as_date(glue::glue("{year}-{month}-01")),
        variable = dplyr::case_when(
          variable == "ET" ~ "m16_et",
          variable == "PET" ~ "m16_pet",
          .default = tolower(variable)
        )
      )
  }) %>%
  dplyr::bind_rows()


dat %>%
  dplyr::select(-year, -month) %>%
  arrow::write_dataset(
    "~/data/zonal",
    format = "parquet",
    partitioning = c("type", "id","variable")
  )

