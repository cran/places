#' @import dplyr
#' @import tidyr
#' @import sp
#' @import rgdal
#' @import geosphere
#' @import googleway
#' @importFrom stringr str_detect
#' @importFrom hms as_hms
#' @importFrom data.table := as.data.table
#' @importFrom rlang sym syms
#' @importFrom stats as.dist cutree dist hclust

# Uncomment if updating packages
# usethis::use_package("dplyr")
# usethis::use_package("tidyr")
# usethis::use_package("sp")
# usethis::use_package("rgdal")
# usethis::use_package("geosphere")
# usethis::use_package("data.table")
# usethis::use_package("rlang")
# usethis::use_package("googleway")
# usethis::use_package("stringr")
# usethis::use_package("stats")
# usethis::use_package("hms")

rna <- 999999
# vs <- NULL
globalVariables(c("accu", "clust", "clust.count", "clust.final", "df1", "hms_local", "lat", "lat.centroid",
                  "lat.centroid.final", "lat_lagged", "lon", "lon.centroid", "lon.centroid.final",
                  "lon_lagged", "placeType", "start_time", "start_time_end", "start_time_lagged",
                  "time.diff.2", "user_id", ".", "vs"))
