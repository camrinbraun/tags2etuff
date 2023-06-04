#' Add temperature to series data, if possible
#'
#' Add missing temperature values to series data from interpolated depth-temperature profile data
#'
#' @param series is output from \code{get_series}
#' @param pdt is output from \code{get_pdt}
#' @param pdt_interp is output from \code{interp_pdt}
#' @param flag is logical indicating whether or not to flag potentially bad interpolated points. This should almost always be TRUE (the default).
#' @return a new series dataframe with additional temperature values where appropriate
#' @export
#'

add_series_temp <- function(series, pdt = NULL, pdt_interp = NULL, flag = TRUE){

  ## nice handling of optional pdt input or if an etuff is provided
  if (is.null(pdt)){
    pdt <- get_pdt(series)
  } else if (!is.null(pdt) & class(pdt) == 'etuff'){
    pdt <- get_pdt(pdt)
  }

  ## nice handling of etuff input instead of series
  if (class(series) == 'etuff') series <- get_series(series)

  ## if pdt_interp isnt provided
  if (is.null(pdt_interp)){
    pdt_interp <- interp_pdt(pdt)
  }

  ## pdt needs doy
  pdt$doy <- lubridate::yday(pdt$DateTime)
  ## pdt needs date as posixct
  pdt$date <- pdt$DateTime

  ## this is a hack of an old function that works well, so fudge a few vars to match that functions requirements
  series$day <- format(series$DateTime_local, '%d-%b-%Y')
  series$date <- series$DateTime_local
  series$doy <- lubridate::yday(series$DateTime_local)
  if (!any(names(series) %in% c('temperature'))) series$temperature <- NA
  start_NAs <- length(which(is.na(series$temperature)))

  series_df <- getSeriesTemp(series, pdt, loess = pdt_interp, flag = flag)

  idx <- which(is.na(series$temperature))
  series$temperature[idx] <- series_df$temperature[idx]

  name_idx <- which(names(series) %in% c('DateTime_local','DateTime','depth','temperature'))
  #series <- series[,name_idx]
  end_NAs <- length(which(is.na(series$temperature)))

  print(paste('Series data started with ', start_NAs, ' NAs and ended with ', end_NAs, ' NAs.', sep=''))

  return(series)

}
