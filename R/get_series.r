#' Extract time series data as typically contained in -Series.csv output from Wildlife Computers
#'
#' Extract time series data as typically contained in -Series.csv output from Wildlife Computers
#'
#' @param etuff is object of class etuff
#' @param temp_res is temporal resolution as valid argument to "by" in \code{seq.POSIXct}
#' @param what_tz indicates what time zone should be used for the series data. If NULL (default), the function will choose for you with some informative printed outputs to let you know whats happening.
#' @return a dataframe of time series data, usually containing at least depth and temperature information
#' @export
#'

get_series <- function(etuff, temp_res = NULL, what_tz = NULL){

  if (class(etuff) != 'etuff' & class(etuff) != 'etuff_archival') stop('Input object must be of class etuff or etuff_archival.')

  meta <- etuff$meta; df <- etuff$etuff

  if (is.null(what_tz)) what_tz <- get_tz(etuff, what_tz)
  if (length(what_tz) > 1){
    what_tz <- Mode(what_tz)
    warning('Multiple tz detected. Using Mode to auto-select one. Pre-specify a tz if you do not want this to happen.')
  }

  ## isolate the appropriate data
  if (class(etuff) == 'etuff_archival'){
    df <- archival_to_etuff(df, vars = c('DateTime','depth','temperature', 'internalTemperature', 'light'))
  }

  series <- df[,c(which(names(df) %in% c('DateTime','depth','temperature', 'internalTemperature', 'light')))]
  if (class(series) != 'data.frame') return(series = NA)
  series <- series[which(!is.na(series$DateTime)),]

  ## if no temporal resolution is specified, try to detect it (this should nearly always work with a PSAT tag)
  if (is.null(temp_res)){
    temp_res <- Mode(as.numeric(diff(series$DateTime)))
    print(paste('No temporal resolution specified. Mode of diff(timeseries) yielded ', temp_res, 'seconds.', sep=''))
  }

  series$depth <- as.numeric(series$depth)
  if (any(names(series) %in% c('temperature'))) series$temperature <- as.numeric(series$temperature)

  if (length(what_tz) > 1){

    stop(paste('Using more than one time zone is not currently supported. Instead, provide input what_tz as one of: ', unique(what_tz), '.', sep=''))

  } else {

    ## setup output dates
    start <- lubridate::with_tz(meta$time_coverage_start, tz = what_tz)
    #start <- lubridate::with_tz(start, tz=what_tz)
    end <- lubridate::with_tz(meta$time_coverage_end, tz = what_tz)
    #end <- lubridate::with_tz(end, tz=what_tz)
    dt_vec <- data.frame(DateTime_local = lubridate::with_tz(seq(start, end, by = temp_res), tz=what_tz))

    ## convert series to local tz
    series$DateTime_local <- lubridate::with_tz(series$DateTime, tzone = what_tz)

  }

  ## merge depth/temp series data onto what full series would look like
  if (class(etuff) != 'etuff_archival') series <- dplyr::left_join(dt_vec, series, by = "DateTime_local")

  return(series)
}
