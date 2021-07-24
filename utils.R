
#' Message Box UI
#'
#' @param head head value
#' @param content content value
#' @param icon_name name of the semantic icon
#' @param color character with colour name
#' @param size character with size
#'
#' @return div with fomantic message
custom_ui_message <- function(head, content, icon_name = "inbox",
                              color = "purple", size = "big") {
  div(class = glue::glue("ui icon {size} {color} message"),
    icon(icon_name),
    div(class = "content",
        div(class = "header", head),
        p(content)
    )
  )
}

#' Load Ship data
#'
#' @param path character with data path
#'
#' @return data.frame with Vessel data
load_ship_data <- function(path = "ships.csv") {
  df_ship  <- data.frame( fread( paste0(path), encoding = 'UTF-8'))
  
  return(df_ship)
}

ship_data <- load_ship_data()


#' Get number of ships by shipType
#'
#' @param ship_Typedata subset of ship for which this is calculated
#' @param raw_value if True it returns raw number, if false it returns
#'
#' @return either character of numeric with total
get_n_ship <- function(ship_Typedata, raw_value = F) {
  length(unique(ship_Typedata$SHIP_ID))
}

#' Get average speed of ships
#'
#' @param ship_Typedata subset of ship for which this is calculated
#' @param raw_value if True it returns raw number, if false it returns
#'
#' @return either character of numeric with total
get_avg_speed_shipType <- function(ship_Typedata, raw_value = T) {
  x <- ship_Typedata$SPEED
  x[is.na(x)] <- 0
  round(mean(x),0)
}

#' Get average LENGTH of ships
#'
#' @param ship_Typedata subset of ship for which this is calculated
#' @param raw_value if True it returns raw number, if false it returns
#'
#' @return either character of numeric with total
get_avg_LENGTH_shipType <- function(ship_Typedata, raw_value = T) {
  x <- ship_Typedata$LENGTH
  x[is.na(x)] <- 0
  round(mean(x),0)
}

#' Get ship Names
#'
#' @param ship_Typedata subset of ship for which this is calculated
#' @param raw_value if True it returns raw number, if false it returns
#'
#' @return either character of numeric with total
get_ship_name <- function(ship_Typedata, raw_value = F) {
 as.list(unique(ship_Typedata$SHIPNAME))
}


#' Get ship Map
#'
#' @param ship_Typedata subset of ship for which this is calculated
#' @param raw_value if True it returns raw number, if false it returns
#'
#' @return either character of numeric with total
get_ship_map <- function(ship_Typedata,raw_value = F) {
  
  avg_isParked <- ship_Typedata %>% summarise(mean(is_parked))
  avg_isParked <- ship_Typedata %>% summarise(mean(is_parked))
  x <- avg_isParked
  x[is.na(x)] <- 2
  x[is.null(x)] <- 2
  if (x==2 ) {
    l <- leaflet(data = data.frame()) %>% addTiles()
  } else{
    if (x < 1) {
      frame <- data.frame(ship_Typedata  %>% filter(SPEED!=0&is_parked==0)) 
      l <- leaflet(data = ship_Typedata) %>% addTiles()  %>%  addPolylines(~LON, ~LAT, weight = 1)  
      
    } else {

      frame <- data.frame(tail(ship_Typedata,1))
      l <- leaflet(data = frame) %>% addTiles() %>% addMarkers(~LON, ~LAT)
      
    }
    
  }
  
  
  rm(frame)
  return(l) 
}


#' Get ship details
#'
#' @param ship_Typedata subset of ship for which this is calculated
#' @param raw_value if True it returns raw number, if false it returns
#'
#' @return either character of numeric with total
get_ship_details <- function(ship_Typedata,raw_value = F) {

    avg_isParked <- ship_Typedata %>% summarise(mean(is_parked))
    x <- avg_isParked
    x[is.na(x)] <- 2
    x[is.null(x)] <- 2
    if (x==2 ) {
      df_ship_details <- data.frame()
    } else{
    
      if (x < 1) {
        df_ship_detail <- data.frame(ship_Typedata %>% 
                                       group_by(SHIPNAME) %>% 
                                       filter(SPEED>2&is_parked==0) %>% 
                                       mutate(lag_LAT = lag(LAT), lag_LON = lag(LON)) %>% 
                                       mutate(distance = diag(distm(cbind(lag_LON, lag_LAT), cbind(LON, LAT), fun = distHaversine)),
                                              timespan = difftime(DATETIME, lag(DATETIME), units = "secs"),
                                              avgspeed = round(mean(SPEED),0)) %>% 
                                       slice_max(distance) %>% 
                                       slice_max(DATETIME) %>% 
                                       ungroup()
        )
        df_ship_detail <- df_ship_detail %>% select(SHIPNAME,FLAG,LENGTH,ship_type,WIDTH,DWT,distance,avgspeed)
        df_ship_detail$distance <- round(df_ship_detail$distance,0)
        colnames(df_ship_detail) <- c("VesselName","Flag","Length (in meters)","VesselType","Width (in meters)","DeadWeight (in tones)","LongSailDistance (in meters)","AverageSpeed (in Knots)")
        df_ship_details <- data.frame(Details = names(df_ship_detail))
        df_ship_details$values <- t(df_ship_detail)
        rm(df_ship_detail)
      } else {
        
        df_ship_detail <- tail(ship_Typedata,1) %>% select(SHIPNAME,FLAG,ship_type)
        colnames(df_ship_detail) <- c("VesselName","Flag","VesselType")
        df_ship_details <- data.frame(Details = names(df_ship_detail))
        df_ship_details$values <- t(df_ship_detail)
        rm(df_ship_detail)
      }
        
    }

    
  
  return(df_ship_details) 
}