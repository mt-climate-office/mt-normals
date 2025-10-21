regions = list(
  blm = "https://mco-normals.s3.us-east-2.amazonaws.com/fgb/blm.fgb",
  counties = "https://mco-normals.s3.us-east-2.amazonaws.com/fgb/counties",
  hucs = "https://mco-normals.s3.us-east-2.amazonaws.com/fgb/hucs.fgb",
  tribes = "https://mco-normals.s3.us-east-2.amazonaws.com/fgb/tribes.fgb"
)

data_dir = "~/data/gridmet/montana"

list.files(data_dir, full.names = T)
