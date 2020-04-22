#' @param etuff is a valid etuff object
#' @param temp_res is temporal resolution as valid argument to "by" in \code{seq.POSIXct}
#' @param what_tz indicates the timezone (usually local time) for this tag dataset. If NULL (default), the function will use the timezone from the tagging location. In the future, this will get more sophisticated by attempting to query tz at each timestamp for associated lat/lon.


get_series <- function(etuff, temp_res = NULL, what_tz = NULL){

  if (class(etuff) != 'etuff') stop('Input object must be of class etuff.')

  meta <- etuff$meta; df <- etuff$etuff

  if (is.null(what_tz)) what_tz <- get_tz(etuff, what_tz)

  ## if no temporal resolution is specified, try to detect it (this should nearly always work with a PSAT tag)
  if (is.null(temp_res)){
    series <- df[,c('datetime','depth','temperature')]
    series <- series[which(!is.na(series$datetime)),]
    temp_res <- Mode(as.numeric(diff(series$datetime)))
    print(paste('No temporal resolution specified. Mode of diff(timeseries) yielded ', temp_res, 'seconds.', sep=''))
  }

  ## get and format series
  ## save a little time by checking here in case series has already been formatted, mostly for large archival tag records
  if (!exists('series')){
    series <- df[,c('datetime','depth','temperature')]
    series <- series[which(!is.na(series$datetime)),]
  }



  if (length(what_tz) > 1){

    stop(paste('Using more than one time zone is not currently supported. Instead, provide input what_tz as one of: ', unique(what_tz), '.', sep=''))

  } else {

    ## setup output dates
    start <- with_tz(meta$time_coverage_start, what_tz)
    end <- with_tz(meta$time_coverage_end, what_tz)
    dt_vec <- data.frame(datetime = seq(start, end, by = temp_res))

    ## convert series to local tz
    series$datetime <- with_tz(series$datetime, tzone = what_tz)

  }

  ## merge depth/temp series data onto what full series would look like
  series <- dplyr::left_join(dt_vec, series, by = "datetime")

  return(series)
}



