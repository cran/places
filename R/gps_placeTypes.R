#' Label each cluster's place type using Google Places API
#'
#' Use get_places() to return the closest place type identified by Google Places API.
#'
#' @param df A dataframe of GPS coordinates as described below
#' @param key A character vector with a Google API key. The default is NULL and must be set by the user.
#' @param radius The maximum radius the Google API should search within for nearby locations. The default is 50m.
#'
#' @return A dataframe with clusters labelled with specific place types (defined by Google) and general categories (defined by package creator)
#'
#' @section Dataframe Requirements:
#' The dataframe needs to have the following named columns:
#' \itemize{
#'   \item lat.centroid.final = latitude coordinates
#'   \item lon.centroid.final = longitude coordinates
#' }
#'
#' @seealso \code{\link{get_clusters}} to cluster GPS coordinates into places.
#' @seealso \code{\link{get_home}} to predict which cluster is an individual's home
#'
#' @examples
#' ## Assume you have run get_clusters() and get_home() on the dataset "places_gps"
#'\dontrun{
#'
#'## Please add your API key from Google - please be aware that this service may cost money.
#'
#' key <- SET_KEY
#'
#' labelled <- get_places(home[[2]], key)
#' }
#' @export

##################################################################################################################

get_places <- function(df, key = NULL, radius = 50){

  if(!all(c("lat.centroid.final", "lon.centroid.final") %in% colnames(df1))) {
    print("Please check your colnames are named according to the Dataframe Requirements noted in documentation. Use ?get_places() for more information.")
  } else {
    if(is.null(key)) {
      print("Please set key. See Google API for more information.")
    } else {
      df.reduced <- df %>%
        select(lat.centroid.final, lon.centroid.final) %>%
        distinct()
      gt.reduced <- googleTypes(df.reduced, key, radius)
      gt <- left_join(df, gt.reduced)
      pt <- placeTypes(gt, df)

      # return(list(gt, pt))
      return(pt)
    }
  }
}
