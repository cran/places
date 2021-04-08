gps_setup <- function(df, max.accu){
  df <- df %>%
    dplyr::filter(accu <= max.accu) %>%
    dplyr::arrange(user_id, start_time) %>%
    dplyr::group_by(user_id) %>%
    dplyr::mutate_at(vars(lat, lon, start_time), funs(lagged = lag(.))) %>%
    dplyr::group_by(user_id) %>%
    dplyr::slice(-1) %>%
    dplyr::group_by(user_id) %>%
    dplyr::mutate(time.diff = difftime(start_time, start_time_lagged, units = "secs"))

  return(df)
}

dist.fx <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
  radians <- pi/180
  lat_to <- lat_to * radians
  lat_from <- lat_from * radians
  lon_to <- lon_to * radians
  lon_from <- lon_from * radians
  dLat <- (lat_to - lat_from)
  dLon <- (lon_to - lon_from)
  a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
  return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
}
