library(magrittr)
library(sf)
library(terra)
library(furrr)
library(future.callr)
library(qs2)

# Sys.setenv(AWS_PROFILE = "my-dev-profile")
# aws sso login --profile my-dev-profile
s3 <- paws::s3(
  config = list(
    credentials = list(
      profile = "my-dev-profile"
    )
  )
)

#' Upload a file to S3 using multipart upload
#'
#' @param client A Paws S3 client object, e.g. from `paws::s3()`.
#' @param file The path to the file to be uploaded.
#' @param bucket The name of the S3 bucket to be uploaded to, e.g. `my-bucket`.
#' @param key The name to assign to the file in the S3 bucket, e.g. `path/to/file`.
upload <- function(client, file, bucket, key) {
  multipart <- client$create_multipart_upload(
    Bucket = bucket,
    Key = key
  )
  resp <- NULL
  on.exit({
    if (is.null(resp) || inherits(resp, "try-error")) {
      client$abort_multipart_upload(
        Bucket = bucket,
        Key = key,
        UploadId = multipart$UploadId
      )
    }
  })
  resp <- try({
    parts <- upload_multipart_parts(client, file, bucket, key, multipart$UploadId)
    client$complete_multipart_upload(
      Bucket = bucket,
      Key = key,
      MultipartUpload = list(Parts = parts),
      UploadId = multipart$UploadId
    )
  })
  return(resp)
}

upload_multipart_parts <- function(client, file, bucket, key, upload_id) {
  file_size <- file.size(file)
  megabyte <- 2^20
  part_size <- 5 * megabyte
  num_parts <- ceiling(file_size / part_size)

  con <- base::file(file, open = "rb")
  on.exit({
    close(con)
  })
  pb <- utils::txtProgressBar(min = 0, max = num_parts)
  parts <- list()
  for (i in 1:num_parts) {
    part <- readBin(con, what = "raw", n = part_size)
    part_resp <- client$upload_part(
      Body = part,
      Bucket = bucket,
      Key = key,
      PartNumber = i,
      UploadId = upload_id
    )
    parts <- c(parts, list(list(ETag = part_resp$ETag, PartNumber = i)))
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  return(parts)
}

find_existing_files <- function(s3) {
  all_files <- character()
  continuation_token <- NULL

  repeat {
    response <- s3$list_objects_v2(
      Bucket = "mco-normals",
      Prefix = "cog/",
      ContinuationToken = continuation_token
    )

    all_files <- c(all_files, sapply(response$Contents, function(x) x$Key))

    if (!response$IsTruncated) break
    continuation_token <- response$NextContinuationToken
  }
  return(basename(all_files))
}

setwd("~/data/gridmet/montana")

existing_files <- find_existing_files(s3)

plan(future.callr::callr,
     workers = parallel::detectCores() - 2)

uploads <-
  list.files(".",
             full.names = TRUE,
             recursive = TRUE, pattern = ".tif") %>%
  stringr::str_subset("normals|aggregated")  %>%
  stringr::str_subset(
    existing_files %>%
      paste(collapse = "|"), negate = T
  ) %>%
  furrr::future_map(\(x){
    tryCatch(
      upload(s3,
             file = x,
             bucket = "mco-normals",
             key = file.path("cog", gsub("^\\./", "", x))),
      # aws_s3$put_object(
      #   Bucket = "skope",
      #   Body = x,
      #   Key = x,
      #   ContentLength = file.size(x),
      #   ChecksumSHA256 = file(x) %>%
      #     openssl::sha256() %>%
      #     openssl::base64_encode()
      # ),
      error = function(e){as.character(e)})

  },
  .env_globals = globalenv(),
  .options = furrr::furrr_options(seed = TRUE,
                                  scheduling = FALSE),

  .progress = TRUE)

plan(sequential)
