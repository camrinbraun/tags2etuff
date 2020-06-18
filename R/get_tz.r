#' Get time zone(s) associated with the eTUFF object of interest
#'
#' Get time zone(s) associated with the eTUFF object of interest
#'
#' @param etuff is object of class etuff
#' @param what_tz is optional specification for time zone to use and bypass the search for one
#' @return a vector containing the time zone(s) relevant to the eTUFF object of interest
#' @export
#'

get_tz <- function(etuff, what_tz = NULL){

  if (class(etuff) != 'etuff' & class(etuff) != 'etuff_archival') stop('Input object must be of class etuff or etuff_archival.')

  meta <- etuff$meta; df <- etuff$etuff

  ## get track to calc time zone(s) if this etuff has a track
  #locs <- get_track(etuff)

  if (class(etuff) == 'etuff_archival'){
    locs <- df %>% filter(VariableName %in% c('DateTime','latitude','longitude')) %>% dplyr::select(-c(id)) %>% dplyr::spread(VariableName, VariableValue)
    locs$longitude <- as.numeric(locs$longitude)
    locs$latitude <- as.numeric(locs$latitude)
    locs <- locs[which(!is.na(locs$latitude)),]
  } else{
    if ('latitude' %in% names(df) & 'longitude' %in% names(df)){
      locs <- df[,c('DateTime','latitude','longitude')]
      locs <- locs[which(!is.na(locs$DateTime) & !is.na(locs$latitude) & !is.na(locs$longitude)),]
      locs$longitude <- as.numeric(locs$longitude)
      locs$latitude <- as.numeric(locs$latitude)
      locs <- locs[which(!is.na(locs$latitude)),]
    }
  }

  ## figure out what time zone(s) to use
  if (!is.null(what_tz)){

    print(paste('Using input time zone for the entire dataset: ', what_tz, '.', sep=''))

  } else if (is.null(what_tz) & (!exists('locs'))){

    what_tz <- lutz::tz_lookup_coords(lat = meta$geospatial_lat_start, lon = meta$geospatial_lon_start, method = "accurate")
    print(paste('No time zone specified and no track in input eTUFF. Time zone for the start location is being used: ', what_tz, sep=''))

  } else if (is.null(what_tz) & (exists('locs'))){

    #locs$day <- as.Date(locs$DateTime)
    #locs <- locs[!duplicated(locs$day),]
    what_tz <- lutz::tz_lookup_coords(lat = locs$latitude, lon = locs$longitude, method = "accurate")
    for (i in 1:length(unique(what_tz))) print(paste('No time zone specified. Detecting the time zone(s) from the tracking data: ', unique(what_tz)[i], '.', sep=''))

  }

  return(what_tz)

}
