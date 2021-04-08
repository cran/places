#' Predict which cluster is an individual's home.
#'
#' @param df1 A dataframe of GPS coordinates as described below.
#' @param df2 A dataframe with named clusters (most likely the dataframe that is returned after running \strong{reduce_multi} OR the places dataframe that is returned after running \strong{get_clusters}).
#' @param home.start A character vector HH:MM:SS which represents the start time that most individuals will be asleep by.
#' @param home.end A character vector HH:MM:SS which represent the start time that most individual may start to wake up by.
#' @param filt A logical T or F if the GPS data should be filtered between home.start and home.end. The default is T.
#' @param max.distance An integer in meters. It is the maximum distance in meters a cluster can be from the home location to be labelled as "home". The defaults is 150 m.
#'
#' @return Returns a list of dataframes. \strong{COUNT} is a dataframe that count how many times an individual was at a clusters  \strong{HOME} is a dataframe with clusters labelled as "Home", "Other", "In Transit"
#'
#' @section Dataframe Requirements:
#' The dataframe needs to have the following named columns:
#' \itemize{
#'   \item user_id = participant id
#'   \item lat = latitude coordinates
#'   \item lon = longitude coordinates
#'   \item start_time = GPS coordinates as POSIXct. Assumes POSIXct variable has been created using UTC timezone.
#'   \item tz_olson_id = local timezone (e.g., EST, America/New_York) as character vector.
#' }
#'
#' @seealso \code{\link{get_clusters}} to cluster GPS coordinates into places.
#' @seealso \code{\link{get_places}} to label each cluster's place type as identified by Google Places API
#'
#' @examples
#' ## Assume you have run get_clusters() on the dataset "places_gps"
#'\dontrun{
#'
#' home <- get_home(places_gps, clusters[[1]], home.start = "21:30:00", home.end = "09:30:00")
#' }
#' @export

################################################################################

get_home <- function(df1, df2, home.start = "00:00:00", home.end = "06:00:00", filt = TRUE, max.distance = 150){

  if(!all(c("user_id", "lat", "lon", "start_time", "tz_olson_id") %in% colnames(df1))) {
    print("Please check your colnames are named according to the Dataframe Requirements noted in documentation. Use ?get_home() for more information.")
  } else {
    if(isTRUE(filt)) {
      df1$tz_olson_id <- as.character(df1$tz_olson_id) # does not work on factors

      gps_split <- split(df1, df1$tz_olson_id)

      for(i in 1:length(gps_split)){

        print(paste("Round 1:", i))

        gps_split[[i]]$time_local <- format(gps_split[[i]]$start_time, usetz = TRUE, tz = gps_split[[i]]$tz_olson_id[1])
      }

      df1_filter <- do.call("rbind", gps_split)
      df1_filter$hms_local <- as_hms(substr(df1_filter$time_local, 12,19))

      if(as_hms(home.start) < as_hms(home.end)) {
        gps_tz_filter <- df1_filter %>% filter(between(hms_local, as_hms(home.start), as_hms(home.end)))
      } else {
        gps_tz_filter1 <- df1_filter %>% filter(between(hms_local, as_hms(home.start), as_hms("23:59:59")))
        gps_tz_filter2 <- df1_filter %>% filter(between(hms_local, as_hms("00:00:00"), as_hms(home.end)))
        gps_tz_filter <- gps_tz_filter1 %>%
          full_join(gps_tz_filter2) %>%
          arrange(start_time)
      }

    } else {
      gps_tz_filter <- df1
    }

    gps_tz_split <- split(gps_tz_filter, gps_tz_filter$user_id)

    xy.list <- list()

    for(i in 1:length(gps_tz_split)){

      print(paste("Round 2:", i))

      df <- gps_tz_split[[i]]

      if(nrow(df) > 1) {

        # convert data to a SpatialPointsDataFrame object
        xy <- SpatialPointsDataFrame(
          matrix(c(df$lon,df$lat), ncol=2), data.frame(ID=seq(1:length(df$lat))),
          proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

        # use the distm function to generate a geodesic distance matrix in meters
        mdist <- distm(xy)

        # cluster all points using a hierarchical clustering approach
        hc <- hclust(as.dist(mdist), method="complete")

        # define the distance threshold, in this case 40 m
        d = max.distance

        # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
        xy$clust <- cutree(hc, h=d)

        # place in list
        xy.list[[i]] <- xy
        gps_tz_split[[i]]$clust.final <- xy$clust

      } else {

        gps_tz_split[[i]]$clust.final <- 0

      }

      gps_tz_split[[i]] <- gps_tz_split[[i]] %>%
        group_by(clust.final) %>%
        mutate(lat.centroid = mean(lat),
               lon.centroid = mean(lon),
               clust.count = n())
    }

    ##################################################################################################################

    # Can't match the 2 x df's on "i" because not everyone has data between certain hours, so some participants get filtered out when splitting

    df2$label <- "Other"
    df2$label[which(df2$clust.final==rna)] <- "In Transit"
    df2$lat.centroid.final[which(df2$clust.final==rna)] <- NA
    df2$lon.centroid.final[which(df2$clust.final==rna)] <- NA
    df2 <- df2 %>%
      distinct()

    df3.list <- list()
    home.count <- list()

    for(i in 1:length(gps_tz_split)){

      print(paste("Round 3:", i))

      home <- gps_tz_split[[i]] %>%
        select(matches("centroid"), clust.count) %>%
        distinct()
      home2 <- home[which.max(home$clust.count),]

      home$user_id <- unique(gps_tz_split[[i]]$user_id)
      home.count[[i]] <- home

      df3 <- df2 %>%
        filter(user_id %in% unique(home$user_id))

      if(nrow(df3) > 0){
        # print(paste0("i = ", i, "; nrow = ", nrow(df3)))

        distance.df <- NULL
        for(j in 1:nrow(df3)){
          if(!is.na(df3$lat.centroid.final[j])){
            distance.df <- c(distance.df, dist.fx(df3$lat.centroid.final[j], df3$lon.centroid.final[j],
                                                  home2$lat.centroid, home2$lon.centroid))
          } else {
            distance.df <- c(distance.df, max.distance + 1000)
          }
        }

        if(min(distance.df) <= max.distance){
          df3$label[which(distance.df == min(distance.df))] <- "Home"
          df3.list[[i]] <- df3
        }
      }
    }

    home.count <- do.call("rbind", home.count)
    df3.all <- do.call("rbind", df3.list)

    results <- list(home.count, df3.all)
    names(results) <- c("count", "home")

    return(results)
  }
}


