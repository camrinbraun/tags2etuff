#' Extract relevant temperatures from interpolated depth-temperature data
#'
#' Extract relevant temperatures from interpolated depth-temperature data. This is used to augment existing time series temperature data (if any).
#'
#' @param series is data frame of Series data unformatted from WC Portal
#'   download
#' @param pdt is data frame of PDT data formatted and output from
#'   \code{prep.loess}
#' @param loess is list output from \code{grid2dloess}
#' @param flag is logical indicating whether or not to mask out flagged values
#'   in LOESS output. Default is TRUE in which flagged values are masked out.
#' @return a data frame of series data with (hopefully) added temperature data from the interpolated temperatures
#' @export

getSeriesTemp <- function(series, pdt, loess, flag = TRUE){

  loess$flag[loess$flag == 0] <- NA

  if(flag) loess$sm_data <- loess$sm_data * loess$flag

  # DEAL WITH THE DATES
  names(series) <- tolower(names(series))
  #series <- series %>% filter(!is.na(depth))
  #series$date <- as.Date(series$day, format = '%d-%b-%Y')
  #ddates = as.POSIXct(series$date,format = findDateFormat(series$date), tz='GMT') #reads dates as dates
  #year = as.numeric(format(series$datetime, '%Y')) #extracts year
  #series$doy = as.numeric(round(julian(ddates, origin = as.Date(paste(year[1],'-01-01',sep = ''))), digits = 0)) #calculate DOY
  series$doy <- lubridate::yday(series$datetime)
  series$row <- round(series$depth, 0) + 1
  series$row[series$row < 0] <- 1

  ## yday / doy calculation was not correct all the time, causing issues. just use lubridate above and its always right.
  series$col <- (series$doy - min(pdt$doy, na.rm=T)) + 1

  ## create dummy series variable to add values to. mostly holdover from old code.
  series.temp <- series

  for(i in 1:length(series.temp[,1])){

    add.tmp <- try(loess$sm_data[series.temp$row[i], series.temp$col[i]], TRUE)
    if (length(add.tmp) == 0){
      series.temp$temperature[i] <- NA
    } else if(class(add.tmp) == 'try-error' | is.na(add.tmp)){
      #add.tmp <- try(mean(loess$sm_data[series.temp$row[c((i-5):(i+5))], series.temp$col[c((i-5):(i+5))]], na.rm=T), TRUE)
      if(class(add.tmp) == 'try-error'){
        series.temp$temperature[i] <- NA
        warning('NA value used because series index was not found in LOESS product. This likely occurs when LOESS interpolated PDT data does not capture a max depth recorded by Series.')
      }
    } else{
      series.temp$temperature[i] <- add.tmp
    }

    if(is.na(series.temp$temperature[i])){
      dt.idx <- which(pdt$date %in% as.Date(series.temp$datetime[i]))
      pdt.i <- pdt[dt.idx,]

      if(length(pdt.i$depth) > 0){
        #extracts depth from tag data for day i
        y <- pdt.i$depth[!is.na(pdt.i$depth)]
        y[y < 0] <- 0

        minZ <- min(pdt.i$depth, na.rm=T)
        maxZ <- max(pdt.i$depth, na.rm=T)
        if(series.temp$depth[i] < minZ) minZ <- series.temp$depth[i]
        if(series.temp$depth[i] > maxZ) maxZ <- series.temp$depth[i]
        predDepth <- seq(minZ, maxZ, by=1)

        #extract temperature from tag data for day i
        # make predictions based on the regression model earlier for the temperature at standard WOA depth levels for low and high temperature at that depth
        suppressWarnings(
          fit.low <- locfit::locfit(pdt.i$mintemp ~ pdt.i$depth)
        )
        suppressWarnings(
          fit.high <- locfit::locfit(pdt.i$maxtemp ~ pdt.i$depth)
        )
        n = length(predDepth)

        pred.low = stats::predict(fit.low, newdata = predDepth, se = F, get.data = T)
        pred.high = stats::predict(fit.high, newdata = predDepth, se = F, get.data = T)

        series.temp$temperature[i] <- (pred.high[which.min((predDepth - series.temp$depth[i])^2)] + pred.low[which.min((predDepth - series.temp$depth[i])^2)]) / 2

      }

    }

  }

  ## put temp values back in series df
  #series$temperature[series.idx] <- series.temp$temperature
  series$temperature <- series.temp$temperature

  #plot(series$depth, pch = 21, bg = series$temperature)

  return(series)
}
