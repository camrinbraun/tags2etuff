#' Add temperature to series data, if possible
#'
#' @param series
#' @param pdt
#' @param pdt_interp

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
  start_NAs <- length(which(is.na(series$temperature)))
  if (!any(names(series) %in% c('temperature'))) series$temperature <- NA
  series_df <- getSeriesTemp(series, pdt, loess = pdt_interp, flag = flag)

  idx <- which(is.na(series$temperature))
  series$temperature[idx] <- series_df$temperature[idx]
  name_idx <- which(names(series) %in% c('DateTime_local','DateTime','depth','temperature'))
  series <- series[,name_idx]
  end_NAs <- length(which(is.na(series$temperature)))

  print(paste('Series data started with ', start_NAs, ' NAs and ended with ', end_NAs, ' NAs.', sep=''))

  return(series)

}
