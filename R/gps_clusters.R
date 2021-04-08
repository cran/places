#' Cluster GPS coordinates into places.
#'
#' Use get_clusters() to cluster a dataframe of GPS coordinates into places.
#'
#' @param df A dataframe of GPS coordinates as described below.
#' @param max.accu An integer in meters. This number means there’s a 68\% probability that the true location is within this radius. The default is 165 m. Any GPS rows with an accuracy higher than this will be dropped.
#' @param max.distance An integer in meters. It is the maximum distance in meters between two points for the pair to be labelled a cluster. The defaults is 150 m.
#' @param max.speed An integer in meters/sec. It is the threshold value that distinguishes a row as Static or Moving. The default is 2.6 meters/sec.
#' @param min.time An integer in minutes. It is the minimum amount of time between two points for the pair to be considered a stationary cluster. The defaults is 3 minutes.
#' @param max.time An integer in minutes. It is the maximum amount of time between two points for the pair to be considered a stationary cluster. The defaults is 15 minutes.
#' @param var.segment If this variable is NOT set, clusters will be created based on the participant’s entire dataset. If this variable is set, clusters will be segmented on the variable. A list can be provided.
#'
#' @return A list containing two named objects. \strong{PLACES} is a dataframe of named clusters and latitude and longitude coordinates for each named cluster that was computed as a weighted average of the original GPS datapoints found within the cluster. The \strong{PLACES} dataframe identifies moving clusters as 999999 \strong{CLUSTERS} is a list of dataframes for each participant that contain the named clusters and coordinates for each original GPS datapoint. Unlike the \strong{PLACES} dataframe, the \strong{CLUSTERS} list labels "moving" clusters as NA.
#'
#' @section Dataframe Requirements:
#' The dataframe needs to have the following named columns:
#' \itemize{
#'   \item user_id = participant id
#'   \item lat = latitude coordinates
#'   \item lon = longitude coordinates
#'   \item start_time = time of GPS coordinates as POSIXct
#' }
#' The dataframe may - but does need to - have the following named columns:
#' \itemize{
#'   \item tz_olson_id = local timezone (only needed if running "get_home")
#'   \item accu = GPS accuracy. This number means there’s a 68\% probability that the true location is within this radius. If this is not available, an accu column will be created and set to 0 so all rows are kept.
#'   \item speed = Speed in meters/sec at which the phone sensing data indicates an individual was moving. If this is not available, speed will be calculated as distance / time between two coordinates.
#'   }
#'
#' @seealso \code{\link{get_home}} to predict which cluster is an individual's home
#' @seealso \code{\link{get_places}} to label each cluster's place type as identified by Google Places API
#'
#' @examples
#' ## Prepare the dataset "places_gps" and run get_clusters()
#'\dontrun{
#'
#' places_gps$time_local <- as.POSIXct(strptime(places_gps$time_local, "%m/%d/%y %H:%M"), tz="UTC")
#'
#' colnames(places_gps)[c(2,4)] <- c("start_time", "lon")
#'
#' clusters <- get_clusters(places_gps)
#' }
#'
#' @export

################################################################################

get_clusters <- function(df, max.accu = 165, max.speed = 2.6, min.time = 3, max.time = 15, max.distance = 150, var.segment = NULL) {

  if(!all(c("user_id", "lat", "lon", "start_time") %in% colnames(df))) {
    print("Please check your colnames are named according to the Dataframe Requirements noted in documentation. Use ?get_clusters() for more information.")
    }

  else{

    # setup gps table
    if(!("accu" %in% colnames(df))) {df$accu <- 0}

    df_setup <- gps_setup(df, max.accu)
    dtHaversine <- data.table::as.data.table(df_setup)

    # return distance in meters
    dtHaversine = {
      dtHaversine[, dist := dist.fx(lat, lon, lat_lagged, lon_lagged)]
    }

    dtHaversine$time.diff <- round(as.numeric(dtHaversine$time.diff)/60, digits = 0)
    dtHaversine$speed.calc <- dtHaversine$dist / dtHaversine$time.diff

    # # create flags for clustering
    # clust.alg <- dtHaversine %>%
    #   dplyr::mutate(time.threshold = case_when(time.diff < min.time ~ "Y",
    #                                     TRUE ~ "N"),
    #          dist.threshold = case_when(dist > max.distance ~ "Y",
    #                                     TRUE ~ "N"),
    #          speed.threshold = case_when(speed > max.speed ~ "Y",
    #                                      TRUE ~ "N"),
    #          speed.calc.threshold = case_when(speed.calc > max.speed ~ "Y",
    #                                           TRUE ~ "N")
    #   ) %>%
    #   dplyr::mutate(clust.update = case_when(dist.threshold == "N" & speed.threshold == "N" ~ 0,
    #                                   TRUE ~ 1))

    # create flags for clustering
    if(length(grep("^speed$", tolower(colnames(dtHaversine)))) > 0) {
      clust.alg <- dtHaversine %>%
        dplyr::mutate(time.threshold = case_when(time.diff > max.time ~ "Y",
                                                 TRUE ~ "N"),
                      dist.threshold = case_when(dist > max.distance ~ "Y",
                                                 TRUE ~ "N"),
                      speed.threshold = case_when(speed > max.speed ~ "Y",
                                                  TRUE ~ "N"),
                      speed.calc.threshold = case_when(speed.calc > max.speed ~ "Y",
                                                       TRUE ~ "N")
        ) %>%
        dplyr::mutate(clust.update = case_when(dist.threshold == "N" & speed.threshold == "N" & time.threshold == "N" ~ 0,
                                               TRUE ~ 1))
    } else {
      clust.alg <- dtHaversine %>%
        dplyr::mutate(time.threshold = case_when(time.diff > max.time ~ "Y",
                                                 TRUE ~ "N"),
                      dist.threshold = case_when(dist > max.distance ~ "Y",
                                                 TRUE ~ "N"),
                      speed.calc.threshold = case_when(speed.calc > max.speed ~ "Y",
                                                       TRUE ~ "N")
        ) %>%
        dplyr::mutate(clust.update = case_when(dist.threshold == "N" & speed.calc.threshold == "N" & time.threshold == "N" ~ 0,
                                               TRUE ~ 1))
    }


    # create initial clusters
    split.clust <- split(clust.alg, clust.alg$user_id)
    clust.df <- list()
    clust.list <- list()
    clust.stop <- list()

    for(i in 1:length(split.clust)){

      test <- split.clust[[i]]

      if(nrow(test) > 1){
        test$clust <- 1
        for(j in 2:nrow(test)){
          if(test$clust.update[[j]] == 1) {
            test$clust[[j]] <- test$clust[[j-1]] + 1
          } else {
            test$clust[[j]] <- test$clust[[j-1]]
          }
        }

        clust.df[[i]] <- test

        test2 <- test %>%
          dplyr::group_by(clust) %>%
          dplyr::mutate(n = n()) %>%
          dplyr::group_by(clust, n) %>%
          dplyr::summarize(lat.centroid = mean(lat),
                           lon.centroid = mean(lon)) %>%
          dplyr::left_join(test)

        if(length(var.segment > 0 )) {
          test3 <- test2 %>%
            dplyr::select(clust, lat.centroid, lon.centroid, user_id, one_of(var.segment)) %>%
            dplyr::distinct()
        } else {
          test3 <- test2 %>%
            dplyr::select(clust, lat.centroid, lon.centroid, user_id) %>%
            dplyr::distinct()
        }

        clust.list[[i]] <- test3

        # getting first row of cluster by ema
        test4a <- test2 %>%
          dplyr::group_by(!!!syms(var.segment), clust) %>%
          dplyr::filter(row_number()==1)

        # getting last row of cluster
        test4b <- test2 %>%
          dplyr::group_by(!!!syms(var.segment), clust) %>%
          dplyr::filter(row_number()==n())

        colnames(test4b)[which(colnames(test4b) == "start_time")] <- "start_time_end"

        test4a$start_time_end <- test4b$start_time_end

        # test4 <- test4a %>%
        #   dplyr::mutate(time.diff.2 = difftime(start_time_end, start_time, units = "secs"))
        #
        # test5 <- test4 %>%
        #   dplyr::filter(time.diff.2 >= min.time*60) %>%
        #   dplyr::filter(time.diff.2 <= max.time*60)

        test4 <- test4a %>%
          dplyr::mutate(time.diff.2 = round(difftime(start_time_end, start_time, units = "mins"), digits = 0))

        test5 <- test4 %>%
          dplyr::filter(time.diff.2 >= min.time)

        clust.stop[[i]] <- test5
      }
    }

    ##################################################################################################################

    xy.list <- list()
    clust.gps <- list()
    clust.final <- list()
    clust.places <- list()

    for(i in 1:length(clust.stop)){

      df <- clust.stop[[i]]

      if(!is.null(df)) {
        if(nrow(df) > 1) {

          # convert data to a SpatialPointsDataFrame object
          xy <- SpatialPointsDataFrame(
            matrix(c(df$lon.centroid,df$lat.centroid), ncol=2), data.frame(ID=seq(1:length(df$lat.centroid))),
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
          clust.stop[[i]]$clust.final <- xy$clust

        } else {

          clust.stop[[i]]$clust.final <- clust.stop[[i]]$clust

        }

        # match with gps
        hold <- clust.stop[[i]] %>%
          dplyr::select(clust, user_id, clust.final)
        clust.gps[[i]] <- clust.list[[i]] %>%
          dplyr::left_join(hold) %>%
          dplyr::select(clust.final, everything())

        final <- dplyr::left_join(clust.df[[i]], clust.gps[[i]])

        # get final list of place stops and traveling points
        clust.final[[i]] <- final %>%
          dplyr::group_by(user_id, clust.final) %>%
          dplyr::mutate(lat.centroid.final = mean(lat.centroid),
                        lon.centroid.final = mean(lon.centroid))

        clust.final[[i]]$lat.centroid.final[which(is.na(clust.final[[i]]$clust.final))] <-
          clust.final[[i]]$lat[which(is.na(clust.final[[i]]$clust.final))]

        clust.final[[i]]$lon.centroid.final[which(is.na(clust.final[[i]]$clust.final))] <-
          clust.final[[i]]$lon[which(is.na(clust.final[[i]]$clust.final))]

        # get final list of place stops for semantic labeling
        clust.places[[i]] <- final %>%
          dplyr::group_by(user_id, clust.final) %>%
          dplyr::summarize(lat.centroid.final = mean(lat.centroid),
                           lon.centroid.final = mean(lon.centroid)) %>%
          dplyr::filter(!is.na(clust.final))
      }
    }

    clust.places.list <- do.call("rbind", clust.places)

    ##################################################################################################################

    clust.ema <- list()

    for(i in 1:length(clust.final)){

      if(!is.null(clust.final[[i]])) {

        # match with ema
        if(length(var.segment > 0 )) {
          hold <- clust.final[[i]] %>%
            dplyr::select(user_id, one_of(var.segment), clust, clust.final, lat.centroid.final, lon.centroid.final)
        } else {
          hold <- clust.final[[i]] %>%
            dplyr::select(user_id, clust, clust.final, lat.centroid.final, lon.centroid.final)
        }
        clust.ema[[i]] <- clust.list[[i]] %>%
          dplyr::left_join(hold) %>%
          dplyr::ungroup() %>%
          dplyr::distinct(across(user_id:lon.centroid.final))
      }
    }

    ema_gps <- do.call("rbind", clust.ema)
    ema_gps <- ema_gps %>%
      tidyr::replace_na(list(clust.final = rna))

    clust.final <- clust.final[lengths(clust.final) > 0]

    results <- list(ema_gps, clust.final)
    names(results) <- c("places", "clusters")
    return(results)

  }
}
