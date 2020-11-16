#' Format Lotek time series
#'
#' Format Lotek time series data from archival tag
#'
#' @param ts is data frame of Lotek's time series file output from \code{read_lotek}
#' @param dates is POSIXct vector (length 2) of release and recovery dates
#' @export

lotek_format_ts <- function(ts){

  ## rename to standard names
  ext_idx <- which(names(ts) %in% c('ExtTemp[C]', 'ExtTempdegC'))
  names(ts)[ext_idx] <- 'temperature'

  int_idx <- which(names(ts) %in% c('IntTemp[C]', 'IntTempdegC'))
  names(ts)[int_idx] <- 'internalTemperature'

  dep_idx <- which(names(ts) %in% c('Depth-dBar', 'Pressure[dBars]'))
  names(ts)[dep_idx] <- 'pressure'

  light_idx <- which(names(ts) %in% c('LightatDepth', 'LightIntensity'))
  names(ts)[light_idx] <- 'light'

  ## deal with dates
  if ('Date' %in% names(ts) & 'Time' %in% names(ts)){
    ts <- ts[which(ts$Date != '' & !is.na(ts$Date)),]
    ts$DateTime <- testDates(paste(ts$Date, ts$Time))
  } else if ('Time(UTC)' %in% names(ts)){
    ts <- ts[which(ts$`Time(UTC)` != '' & !is.na(ts$`Time(UTC)`)),]
    ts$DateTime <- testDates(ts$`Time(UTC)`)
  } else if ('Timestamp' %in% names(ts)){
    ts <- ts[which(ts$Timestamp != '' & !is.na(ts$Timestamp)),]
    ts$DateTime <- testDates(ts$Timestamp)
  }
  if (all(ts$DateTime < dates[1]) | all(ts$DateTime > dates[2]))
    stop('Error parsing time series dates.')

  ## filter (dates, bad data, etc)
  ts.new <- ts[which(ts$DateTime >= dates[1] & ts$DateTime <= dates[2]),]

  ## reshape
  ts.new <- reshape2::melt(ts.new, id.vars=c('DateTime'), measure.vars = c('temperature','internalTemperature','pressure','light'))
  ts.new$VariableName <- ts.new$variable

  ## merge with observation types
  ts.new <- merge(x = ts.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
  ts.new <- ts.new[,c('DateTime','VariableID','value','VariableName','VariableUnits')]

  ## finish formatting/filtering after standardization
  names(ts.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
  ts.new <- ts.new[order(ts.new$DateTime, ts.new$VariableID),]
  ts.new$DateTime <- as.POSIXct(ts.new$DateTime, tz='UTC')
  ts.new$DateTime <- format(ts.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
  ts.new <- ts.new[which(!is.na(ts.new$VariableValue)),]
  ts.new <- ts.new[which(ts.new$VariableValue != ' '),]

  return(ts.new)
}
