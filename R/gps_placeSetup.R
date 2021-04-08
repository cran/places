#### This location (39.11281,	-84.50238) does not follow pattern of results. Return multiple route/POI before returning place type.

##### 1. Function to get place names & place types using Google's API
# Assumes dataframe with separate numeric columns named latitude, longitude

# Google API Function

googleTypes <- function(df, key, r){

  if(is.null(key)) {
    print("Please set key. See Google API for more information.")
  } else {

    # prepare dataframe with new columns
    df$placeName <- "Unknown" # used to be NULL
    df$placeType <- "Unknown" # used to be NULL
    # df$placeCategory <- "Unknown" # move to second fx

    # loop through dataframe to extract information using Google API
    for(i in 1:nrow(df)){
      print(i)
      # if(df$label[i] != "In Transit"){
      if(!is.na(df$lat.centroid.final[i])){

        # getting Google API results
        place <- google_places(location=c(df$lat.centroid.final[i],
                                          df$lon.centroid.final[i]),
                               radius=r,
                               key=key)

        # checking if API results returned without error
        if(place$status != "OK" & place$status != "ZERO_RESULTS"){
          print("Stop! There's an error")
          print(i)
        }

        # check if API results returned any results
        if (length(place$results) == 0){
          df$placeName[i] <- "Not Found"
          df$placeType[i] <- "Not Found"
        }

        # if API returned results > 2, extract commercial place name and place type
        else if(length(place$results$name) > 2) {
          # when results are 3, result 1 = address, result 3 = locality,
          # so we will select result 2 as most likely business name
          df$placeName[i] <- place$results$name[2]
          df$placeType[i] <- place$results$types[2]

          # if API returned results < 3, extract address and place type
        } else if (length(place$results$name) < 3) {
          # when results are 2, 1 = address, result 2 = locality,
          # so selecting address, but perhaps for security, would want result 2 instead
          df$placeName[i] <- place$results$name[1]
          df$placeType[i] <- place$results$types[1]
        }

        # removing POI and Establishment if other non-generic place-types are available
        if(length(df$placeType[[i]]) > 2) {
          df$placeType[[i]] <- df$placeType[[i]][df$placeType[[i]] != "point_of_interest"]
          df$placeType[[i]] <- df$placeType[[i]][df$placeType[[i]] != "establishment"]
        }

        # selecting first place type in list
        df$placeType[i] <- df$placeType[i][[1]][1]

      }
    }

    return(df)

  }
}

#################################################################################################################

##### 2. Function to identify place categories

placeTypes <- function(df1, df2) {

  health <- df1 %>%
    filter(str_detect(placeType, "doctor|hospital|physiotherapist|health|pharmacy|dentist"))
  health$placeCategory <- "Health"

  selfCare <- df1 %>%
    filter(str_detect(placeType, "spa|gym|beauty_salon|hair_care"))
  selfCare$placeCategory <- "Self Care"

  transit <- df1 %>%
    filter(str_detect(placeType, "bus_station|transit_station|airport|subway_station|gas_station|parking"))
  transit$placeCategory <- "Transit"

  shopping <- df1 %>%
    filter(str_detect(placeType, "mall|electronics_store|home_goods_store|hardware_store|clothing_store|department_store|furniture_store|electronics_store|department_store|shoe_store|jewelry_store|car_dealer|bicycle_store|book_store|pet_store|^store"))
  shopping$placeCategory <- "Shopping"

  food <- df1 %>%
    filter(str_detect(placeType, "grocery|supermarket|convenience_store|liquor_store"))
  food$placeCategory <- "Food/Bev Shopping"

  cafe <- df1 %>%
    filter(str_detect(placeType, "cafe|restaurant|meal_takeaway|bar|night_club|meal_delivery|bakery|food"))
  cafe$placeCategory <- "Restaurant/Bar"

  services <- df1 %>%
    filter(str_detect(placeType, "real_estate_agency|general_contractor|car_repair|insurance_agency|lawyer|electrician|moving_company|funeral_home|car_wash|painter|car_rental|plumber|florist|roofing_contractor|laundry|veterinary_care|storage|travel_agency"))
  services$placeCategory <- "Professional Services"

  govt <- df1 %>%
    filter(str_detect(placeType, "local_government_office|courthouse|post_office|fire_station"))
  govt$placeCategory <- "Government Services"

  money <- df1 %>%
    filter(str_detect(placeType, "atm|finance|accounting|bank"))
  money$placeCategory <- "Money Services"

  religious <- df1 %>%
    filter(str_detect(placeType, "church|place_of_worship|synagogue"))
  religious$placeCategory <- "Religious Services"

  edu <- df1 %>%
    filter(str_detect(placeType, "school|library|university"))
  edu$placeCategory <- "Education"

  eduEnt <- df1 %>%
    filter(str_detect(placeType, "museum|art_gallery"))
  eduEnt$placeCategory <- "Museums/Galleries"

  ent <- df1 %>%
    filter(str_detect(placeType, "stadium|tourist_attraction|movie_theater|movie_rental|casino|bowling_alley|amusement_park|natural_feature|^park$"))
  ent$placeCategory <- "Entertainment"

  lodging <- df1 %>%
    filter(str_detect(placeType, "rv_park|lodging|neighborhood"))
  lodging$placeCategory <- "Lodging"

  misc <- df1 %>%
    filter(str_detect(placeType, "Not Found|route|point_of_interest|premise|locality|political"))
  misc$placeCategory <- "Miscellaneous"

  dataCat <- rbind(cafe,edu,eduEnt,ent,food,govt,health,lodging,misc,money,
                   religious,selfCare,services,shopping,transit)

  dataCat <- left_join(df1, dataCat)
  df <- df2 %>%
    left_join(dataCat) %>%
    tidyr::replace_na(list(placeCategory = "Unknown"))

  df$placeName[which(df$label == "In Transit")] <- "In Transit"
  df$placeType[which(df$label == "In Transit")] <- "In Transit"
  df$placeCategory[which(df$label == "In Transit")] <- "In Transit"

  df <- df %>% distinct()
  df$placeType <- unlist(df$placeType)

  return(df)

}
