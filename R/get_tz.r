get_tz <- function(etuff, what_tz = NULL){

  if (class(etuff) != 'etuff') stop('Input object must be of class etuff.')

  meta <- etuff$meta; df <- etuff$etuff

  ## get track to calc time zone(s) if this etuff has a track
  if ('latitude' %in% names(df) & 'longitude' %in% names(df)){
    locs <- df[,c('datetime','latitude','longitude')]
    locs <- locs[which(!is.na(locs$datetime) & !is.na(locs$latitude) & !is.na(locs$longitude)),]

  }


  ## figure out what time zone(s) to use
  if (!is.null(what_tz)){

    print(paste('Using input time zone for the entire dataset: ', what_tz, '.', sep=''))

  } else if (is.null(what_tz) & (!('latitude' %in% names(df)) | !('longitude' %in% names(df)))){

    what_tz <- lutz::tz_lookup_coords(lat = meta$geospatial_lat_start, lon = meta$geospatial_lon_start, method = "accurate")
    print(paste('No time zone specified and no track in input eTUFF. Time zone for the start location is being used: ', what_tz, sep=''))

  } else if (is.null(what_tz) & ('latitude' %in% names(df) & 'longitude' %in% names(df))){

    locs$day <- as.Date(locs$datetime)
    locs <- locs[!duplicated(locs$day),]
    what_tz <- lutz::tz_lookup_coords(lat = locs$latitude, lon = locs$longitude, method = "accurate")
    print(paste('No time zone specified. Detecting and using the time zone(s) from the tracking data. Unique time zones applied to the dataset are: ', unique(what_tz), '.', sep=''))

  }

  return(what_tz)

}
