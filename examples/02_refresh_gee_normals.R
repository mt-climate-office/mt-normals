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

cover = ee$ImageCollection("projects/rangeland-analysis-platform/vegetation-cover-v3")$filterDate("1991-01-01", "2024-12-31")$mean() %>%
  rgee::ee_as_rast(region = shp$geometry(), via="drive", scale=100)%>%
  normals::write_as_cog("./cover.tif")
npp = ee$ImageCollection("projects/rangeland-analysis-platform/npp-partitioned-v3")$filterDate("1991-01-01", "2024-12-31")$mean() %>%
  rgee::ee_as_rast(region = shp$geometry(), via="drive", scale=100) %>%
  normals::write_as_cog("./npp.tif")

process_to_normal_cogs <- function(r, out_dir = "~/data/blm_project/gee_data", scale_factor=0.0001) {
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
      func = ifelse(v %in% c("et", "pet", "gpp"), "sum", "mean"),
    ) %>%
    dplyr::group_by(v, func) %>%
    dplyr::summarise(r = list(terra::rast(f)),
                     dir_path = unique(dirname(f))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(r = list(terra::app(r, func, na.rm = T) %>%
                             magrittr::set_names("mean") %>%
                             normals::write_as_cog(file.path(dir_path, "annual_mean.tif"))))
}


ee_zonal_mean <- function(ee_raster, zones, scale = 30, max_pixels = 1e9,
                          geometry_col = "geometry", id_col = NULL) {

  # Convert to ee.Image if it's an ImageCollection
  if (inherits(ee_raster, "ee.imagecollection.ImageCollection")) {
    ee_raster <- ee_raster$toBands()
  }

  # Get band names
  band_names <- ee_raster$bandNames()$getInfo()

  # Function to calculate zonal statistics for each feature
  zonal_stats <- function(feature) {
    # Calculate mean for all bands within the feature geometry
    stats <- ee_raster$reduceRegion(
      reducer = ee$Reducer$mean(),
      geometry = feature$geometry(),
      scale = scale,
      maxPixels = max_pixels
    )

    # Add the statistics as properties to the feature
    feature$set(stats)
  }

  # Map the zonal statistics function over all features
  zones_with_stats <- zones$map(rgee::ee_utils_pyfunc(zonal_stats))

  # Convert to R data frame
  result <- rgee::ee_as_sf(zones_with_stats)

  # Clean up the result - remove geometry and reshape if needed
  result_clean <- result %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble()

  # If id_col is specified, make sure it's included
  if (!is.null(id_col) && id_col %in% names(result_clean)) {
    result_clean <- result_clean %>%
      dplyr::select(!!id_col, dplyr::everything())
  }

  return(result_clean)
}
