
get_srss <- function(etuff, series = NULL){

  if (is.null(series)) series <- get_series(etuff)

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

  return(srss)
}
