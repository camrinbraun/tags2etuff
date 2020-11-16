#' Format Lotek daily log
#'
#' Format Lotek daily log data from archival tag
#'
#' @param dl is data frame of Lotek's daily log file output from \code{read_lotek}
#' @param dates is POSIXct vector (length 2) of release and recovery dates
#' @export

lotek_format_dl <- function(dl, dates){

  measure.vars <- c()

  ## rename to standard names
  sst_idx <- which(names(dl) %in% c('SSTMin'))
  names(dl)[sst_idx] <- 'sstMin'; rm(sst_idx)
  if (any(names(dl) %in% 'sstMin'))
    measure.vars[length(measure.vars) + 1] <- 'sstMin'

  sst_idx <- which(names(dl) %in% c('SSTMedian'))
  names(dl)[sst_idx] <- 'sstMedian'; rm(sst_idx)
  if (any(names(dl) %in% 'sstMedian'))
    measure.vars[length(measure.vars) + 1] <- 'sstMedian'

  sst_idx <- which(names(dl) %in% c('SSTMax'))
  names(dl)[sst_idx] <- 'sstMax'; rm(sst_idx)
  if (any(names(dl) %in% 'sstMax'))
    measure.vars[length(measure.vars) + 1] <- 'sstMax'

  sst_idx <- which(names(dl) %in% c('SST1[C]','SST1.C.'))
  names(dl)[sst_idx] <- 'sst'
  if (any(names(dl) %in% 'sst'))
    measure.vars[length(measure.vars) + 1] <- 'sst'

  temp_idx <- which(names(dl) %in% c('MaxTemp'))
  names(dl)[temp_idx] <- 'tempMax'
  if (any(names(dl) %in% 'tempMax'))
    measure.vars[length(measure.vars) + 1] <- 'tempMax'

  dep_idx <- which(names(dl) %in% c('SST1Depth[dBars]',	'SST1Depth.dBars.', 'DepthForSST'))
  names(dl)[dep_idx] <- 'sstDepth'; rm(dep_idx)
  if (any(names(dl) %in% 'sstDepth'))
    measure.vars[length(measure.vars) + 1] <- 'sstDepth'

  lat_idx <- which(names(dl) %in% c('Latitude[degs]',	'Latitude.degs.', 'Latitude(-3.44elevation)'))
  names(dl)[lat_idx] <- 'latitude'
  if (any(names(dl) %in% 'latitude'))
    measure.vars[length(measure.vars) + 1] <- 'latitude'

  lon_idx <- which(names(dl) %in% c('Longitude[degs]', 'Longitude.degs.', 'Longitude'))
  names(dl)[lon_idx] <- 'longitude'
  if (any(names(dl) %in% 'longitude'))
    measure.vars[length(measure.vars) + 1] <- 'longitude'

  dep_idx <- which(names(dl) %in% c('MinPress[dBars]', 'MinPress.dBars.', 'MinDepth'))
  names(dl)[dep_idx] <- 'depthMin'; rm(dep_idx)
  if (any(names(dl) %in% 'depthMin'))
    measure.vars[length(measure.vars) + 1] <- 'depthMin'

  dep_idx <- which(names(dl) %in% c('MaxPress[dBars]', 'MaxPress.dBars.', 'MaxDepth'))
  names(dl)[dep_idx] <- 'depthMax'
  if (any(names(dl) %in% 'depthMax'))
    measure.vars[length(measure.vars) + 1] <- 'depthMax'

  dl$sst[which(dl$sst < -2 | dl$sst > 36)] <- NA
  dl$sstDepth[which(dl$sstDepth > 10)] <- NA

  ## sunrise
  if (any(names(dl) %in% c('Sunrise', 'SunriseUTC'))){

    sr_idx <- which(names(dl) %in% c('Sunrise', 'SunriseUTC'))
    day_idx <- which(names(dl) %in% c('Date', 'MissionDate'))
    sr <- dl[,c(day_idx, sr_idx)]
    warning('depthSunrise being fixed at 0.')
    sr$depthSunrise <- 0

    if (any(names(sr) %in% c('Sunrise'))){
      if (is.numeric(sr$Sunrise)){ ## convert numeric to time
        sr$DateTime <- testDates(sr[,1]) + as.numeric(sr$Sunrise * 60, units='secs')
      }
    } else{
      sr$DateTime <- testDates(paste(sr[,1], sr[,2]))
    }
    sr <- sr[,c('DateTime','depthSunrise')]

    ## reshape sr
    sr <- reshape2::melt(sr, id.vars=c('DateTime'), measure.vars = c('depthSunrise'))
    sr$VariableName <- sr$variable

    ## merge with observation types
    sr <- merge(x = sr, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
    sr <- sr[,c('DateTime','VariableID','value','VariableName','VariableUnits')]

    ## finish formatting/filtering after standardization
    names(sr) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')

  }

  ## sunset
  if (any(names(dl) %in% c('Sunset', 'SunsetUTC'))){

    ss_idx <- which(names(dl) %in% c('Sunset', 'SunsetUTC'))
    day_idx <- which(names(dl) %in% c('Date', 'MissionDate'))
    ss <- dl[,c(day_idx, ss_idx)]
    warning('depthSunset being fixed at 0.')
    ss$depthSunset <- 0

    if (any(names(ss) %in% c('Sunset'))){
      if (is.numeric(ss$Sunset)){ ## convert numeric to time
        ss$DateTime <- testDates(ss[,1]) + as.numeric(ss$Sunset * 60, units='secs')
      }
    } else{
      ss$DateTime <- testDates(paste(ss[,1], ss[,2]))
    }
    ss <- ss[,c('DateTime','depthSunset')]

    ## reshape ss
    ss <- reshape2::melt(ss, id.vars=c('DateTime'), measure.vars = c('depthSunset'))
    ss$VariableName <- ss$variable

    ## merge with observation types
    ss <- merge(x = ss, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
    ss <- ss[,c('DateTime','VariableID','value','VariableName','VariableUnits')]

    ## finish formatting/filtering after standardization
    names(ss) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')

  }

  ## deal with dates
  dl <- dl[which(dl$Date != '' & !is.na(dl$Date)),]
  dl$DateTime <- testDates(paste(dl$Date, dl$Time))

  if (all(dl$DateTime < dates[1]) | all(dl$DateTime > dates[2]))
    stop('Error parsing time series dates.')
  #measure.vars[length(measure.vars) + 1] <- 'DateTime'

  nms <- names(dl)
  warning('The following variables are NOT being included in the resulting eTUFF file:')
  warning(nms[which(!(nms %in% measure.vars))])

  ## filter (dates, bad data, etc)
  dl.new <- dl[which(dl$DateTime >= dates[1] & dl$DateTime <= dates[2]),]

  ## reshape
  dl.new <- reshape2::melt(dl.new, id.vars=c('DateTime'), measure.vars = measure.vars)
  dl.new$VariableName <- dl.new$variable

  ## merge with observation types
  dl.new <- merge(x = dl.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
  dl.new <- dl.new[,c('DateTime','VariableID','value','VariableName','VariableUnits')]

  ## finish formatting/filtering after standardization
  names(dl.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')

  ## rbind
  if (exists('ss')) dl.new <- rbind(dl.new, ss)
  if (exists('sr')) dl.new <- rbind(dl.new, sr)

  dl.new <- dl.new[order(dl.new$DateTime, dl.new$VariableID),]
  dl.new$DateTime <- as.POSIXct(dl.new$DateTime, tz='UTC')
  dl.new$DateTime <- format(dl.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
  dl.new <- dl.new[which(!is.na(dl.new$VariableValue)),]
  dl.new <- dl.new[which(dl.new$VariableValue != ' '),]

  return(dl.new)
}

