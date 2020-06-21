#' Add day night designation to series
#'
#' Add day night designation to series based on local sunrise sunset times
#'
#' @param series is input time series data
#' @param etuff is etuff object
#'
#' @return a vector of day/night labels that coincides with input time series
#' @export
#'
#' @importFrom lubridate %within%

add_daynight <- function(series, etuff){

  #if (is.null(series)) series <- get_series(etuff)

  ## get sunrise/sunset times
  srss <- get_srss(etuff)

  ## designate day/night
  dt_local <- series$DateTime_local
  dn <- sapply(dt_local, function(x, srss){
    dn <- ifelse(any(x %within% srss$day_interval), 'd', 'n')
    if (dn == 'n') dn <- ifelse(any(x %within% srss$night_interval), 'n', NA)
    return(dn)
  }, srss=srss)

  return(dn)
}


