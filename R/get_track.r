#' @param etuff is a valid etuff object
#' @param what_tz indicates the timezone (usually local time) for this tag dataset. If NULL (default), the function will use the timezone from the tagging location. In the future, this will get more sophisticated by attempting to query tz at each timestamp for associated lat/lon.


get_track <- function(etuff, what_tz = NULL){

  if (class(etuff) != 'etuff') stop('Input object must be of class etuff.')

  meta <- etuff$meta; df <- etuff$etuff

  ## get and format track
  idx <- which(names(df) %in% c('DateTime','latitude','longitude','latitudeError','longitudeError','argosLC'))
  tr <- df[,idx]
  tr <- tr[which(!is.na(tr$DateTime)),]
  tr <- tr[which(!is.na(tr$latitude)),]

  ## deal with timezones
  if (is.null(what_tz)) what_tz <- get_tz(etuff, what_tz)
  tr$tz <- what_tz
  for (i in 1:nrow(tr)) tr$localHour[i] <- lubridate::hour(with_tz(tr$DateTime[i], tr$tz[i]))

  return(tr)
}
