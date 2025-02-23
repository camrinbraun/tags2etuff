#' Get sunrise/sunset times
#'
#' Get sunrise/sunset times concurrent to the timeseries data in eTUFF. Typically this is used to label time series information with relevant SRSS times.
#'
#' @param etuff is object of class etuff
#' @return a dataframe of sunrise and sunset information
#' @export
#'

get_srss <- function(etuff){

  tr <- get_track(etuff)
  tr$day <- as.Date(tr$DateTime)

  srss <- data.frame(tr %>% group_by(day) %>% summarise(latitude = mean(latitude), longitude = mean(longitude), tz = unique(tz)[1], n_locs = n()))
  srss$DateTime <- as.POSIXct(format(srss$day, '%Y-%m-%d'), tz='UTC')

  ## add srss date-times
  for (i in 1:nrow(srss)){

    sr <- maptools::sunriset(cbind(srss$longitude[i], srss$latitude[i]), lubridate::with_tz(srss$DateTime[i], srss$tz[i]), direction = "sunrise", POSIXct.out = TRUE)$time
    if (as.Date(substr(sr, 1, 10)) != as.Date(srss$DateTime[i])){
      sr <- maptools::sunriset(cbind(srss$longitude[i], srss$latitude[i]), srss$DateTime[i], direction = "sunrise", POSIXct.out = TRUE)$time
      sr <- lubridate::with_tz(sr, srss$tz[i])
    }

    ss <- maptools::sunriset(cbind(srss$longitude[i], srss$latitude[i]), lubridate::with_tz(srss$DateTime[i], srss$tz[i]), direction = "sunset", POSIXct.out = TRUE)$time
    if (as.Date(substr(ss, 1, 10)) != as.Date(srss$DateTime[i])){
      ss <- maptools::sunriset(cbind(srss$longitude[i], srss$latitude[i]), srss$DateTime[i], direction = "sunset", POSIXct.out = TRUE)$time
      ss <- lubridate::with_tz(ss, srss$tz[i])
    }

    if (i == 1){
      srss$sunrise <- sr
      srss$sunset <- ss

    } else{
      srss$sunrise[i] <- sr
      srss$sunset[i] <- ss

    }

  }

  srss$day_interval <- lubridate::interval(srss$sunrise, srss$sunset)

  suppressWarnings(srss$night_interval <- lubridate::interval(srss$sunset, srss$sunrise[2:nrow(srss)]))
  srss$night_interval[nrow(srss)] <- NA

  return(srss)
}

