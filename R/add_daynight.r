add_daynight <- function(series, etuff){

  #if (is.null(series)) series <- get_series(etuff)

  ## get sunrise/sunset times
  srss <- get_srss(etuff, series = series)
  srss$day_interval <- interval(srss$sunrise, srss$sunset)

  suppressWarnings(srss$night_interval <- interval(srss$sunset, srss$sunrise[2:nrow(srss)]))
  srss$night_interval[nrow(srss)] <- NA

  ## designate day/night
  dt_local <- series$DateTime_local
  dn <- sapply(dt_local, function(x, srss){
    dn <- ifelse(any(x %within% srss$day_interval), 'd', 'n')
    if (dn == 'n') dn <- ifelse(any(x %within% srss$night_interval), 'n', NA)
    return(dn)
  }, srss=srss)

  return(dn)
}


