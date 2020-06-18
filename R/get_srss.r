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
    if (i == 1){
      srss$sunrise <- maptools::sunriset(cbind(srss$longitude[i], srss$latitude[i]), with_tz(srss$DateTime[i], srss$tz[i]), direction = "sunrise", POSIXct.out = TRUE)$time
      srss$sunset <- maptools::sunriset(cbind(srss$longitude[i], srss$latitude[i]), with_tz(srss$DateTime[i], srss$tz[i]), direction = "sunset", POSIXct.out = TRUE)$time

    } else{
      srss$sunrise[i] <- maptools::sunriset(cbind(srss$longitude[i], srss$latitude[i]), with_tz(srss$DateTime[i], srss$tz[i]), direction = "sunrise", POSIXct.out = TRUE)$time
      srss$sunset[i] <- maptools::sunriset(cbind(srss$longitude[i], srss$latitude[i]), with_tz(srss$DateTime[i], srss$tz[i]), direction = "sunset", POSIXct.out = TRUE)$time

    }

  }

  srss$day_interval <- interval(srss$sunrise, srss$sunset)

  suppressWarnings(srss$night_interval <- interval(srss$sunset, srss$sunrise[2:nrow(srss)]))
  srss$night_interval[nrow(srss)] <- NA

  return(srss)
}

