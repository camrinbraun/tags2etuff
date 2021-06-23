#' Get track data from eTUFF
#'
#' Extract track data from eTUFF
#'
#' @param etuff is object of class etuff
#' @param what_tz indicates the timezone (usually local time) for this tag dataset. If NULL (default), the function will use the timezone from the tagging location. In the future, this will get more sophisticated by attempting to query tz at each timestamp for associated lat/lon.
#' @return a dataframe of lat/lon and (sometimes) associated error
#' @export

get_track <- function(etuff, what_tz = NULL,...){

  if (class(etuff) != 'etuff' & class(etuff) != 'etuff_archival') stop('Input object must be of class etuff or etuff_archival.')

  meta <- etuff$meta;

  ## isolate the appropriate data
  if (class(etuff) == 'etuff_archival'){
    df <- etuff$etuff
    tr <- archival_to_etuff(df, vars = c('DateTime','latitude','longitude','latitudeError','longitudeError','argosLC'))
  } else{
    df <- data.frame(etuff$etuff)
    idx <- which(names(df) %in% c('DateTime','latitude','longitude','latitudeError','longitudeError','argosLC'))
    tr <- df[,idx]
  }

  tr <- tr[which(!is.na(tr$DateTime)),]

  idx <- which(names(tr) %in% c('latitude','longitude','latitudeError','longitudeError'))
  for (i in idx) tr[,i] <- as.numeric(tr[,i])
  tr <- tr[which(!is.na(tr$latitude)),]
  tr <- tr %>% filter(latitude < 90 & latitude > -90)

  ## deal with timezones
  if (is.null(what_tz)) what_tz <- get_tz(etuff, what_tz)
  tr$tz <- what_tz
  for (i in 1:nrow(tr)) tr$localHour[i] <- lubridate::hour(lubridate::with_tz(tr$DateTime[i], tr$tz[i]))

  return(tr)
}
