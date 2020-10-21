#' Convert satellite tag data to etuff
#'
#' Convert satellite tag data to etuff. Currently this function has good support for Wildlife Computers tags and is in development for others such as Lotek and MT.
#'
#' @param dir is directory the target data is stored in
#' @param manufacturer is character indicating tag manufacturer. Choices are
#'   'Wildlife','Microwave','Lotek'.
#' @param tagtype is character. Choices are 'PSAT', 'SPOT', ...
#' @param fName is character indicating the file name of interest to read. This
#'   is currently only required for reading archival data from Microwave or Lotek
#'   tags. It can also be used to read individual files from WC or custom inputs. See `customCols` parameter.
#' @param dates is POSIXct vector of length 2 indicating start and stop dates
#'   for the tag data of interest
#' @param tatBins is integer or numeric vector indicating the
#'   time-at-temperature summary bins for a Wildlife tag. Length will usually be
#'   about 14-16 bins. Defaults to NULL and the function tries to read bins from
#'   the file.
#' @param tadBins is integer or numeric vector indicating the time-at-depth
#'   summary bins for a Wildlife tag. Length will usually be about 14-16 bins.
#'   Defaults to NULL and the function tries to read bins from the file.
#' @param obsTypes is csv sourced from github containing the latest obsTypes
#'   recognized by the NASA OIIP project. Usually this is left NULL and the file
#'   is auto-magically downloaded for you. The only reason you may want to
#'   specify this would be in order to work offline.
#' @param check_meta is logical indicating whether or not to check the etuff file metadata
#' @param customCols is optional argument that allows custom specification of input columns for input \code{fName}. these custom specs must match the accepted obsTypes
#' @export

tag_to_etuff <- function(dir, meta_row, fName = NULL, tatBins = NULL, tadBins = NULL,
                         obsTypes = NULL, check_meta = TRUE,...){

  #if (check_meta) build_meta_head(meta_row = meta_row, write_hdr = F)

  args <- list(...)
  if ('fName' %in% names(args)) fName <- args$fName
  if ('customCols' %in% names(args)) customCols <- args$customCols
  if ('write_direct' %in% names(args)) write_direct <- args$write_direct
  if ('etuff_file' %in% names(args)) etuff_file <- args$etuff_file

  if ('manufacturer' %in% names(args)){
    manufacturer <- args$manufacturer
  } else {
    manufacturer <- meta_row$manufacturer
  }

  ## need tag type
  if ('tagtype' %in% names(args)){
    tagtype <- args$tagtype
  } else{
    tagtype <- meta_row$model
  }

  ## start and end dates
  if ('dates' %in% names(args)){
    dates <- args$dates
  } else{
    if (!lubridate::is.POSIXct(meta_row$time_coverage_start) | !lubridate::is.POSIXct(meta_row$time_coverage_end)) stop('Start and end times specified by meta_row must be of class POSIXct.')
    dates <- c(meta_row$time_coverage_start, meta_row$time_coverage_end)
  }

  ## get gpe3 logical
  if ('gpe3' %in% names(args)){
    gpe3 <- args$gpe3
  } else if(!is.na(meta_row$waypoints_method)){
    if (meta_row$waypoints_method == 'GPE3') gpe3 <- TRUE
  } else{
    gpe3 <- FALSE
  }

  #------------------------
  ## checking before we start
  #------------------------

  if (is.null(obsTypes)){
    # try to get it from github
    print('Getting obsTypes...')
    url <- "https://raw.githubusercontent.com/camrinbraun/tagbase/master/eTUFF-ObservationTypes.csv"
    obsTypes <- try(read.csv(text=RCurl::getURL(url)), TRUE)

    # if that doesnt work, kill the funciton
    if (class(obsTypes) == 'try-error') stop(paste('obsTypes not specified in function call and unable to automatically download it from github at', url, sep=' '))

  }

  # check the specific manufacturer is actually supported
  #print(manufacturer); print(!exists('customCols'))
  if (manufacturer == 'unknown'){
    if(!exists('customCols')) stop('if manufacturer is unknown, customCols must be specified.')
  } else if (!(manufacturer %in% c('Microwave','Wildlife','Wildlife Computers', 'Lotek'))){
    print('entering 2')
    stop('the specified manufacturer is not supported.')
  }

  # check and coerce allowable tag types
  if (tagtype %in% c('spot','SPOT','SPOT-F','mrPATspot','spot380','spot258','towed SPOT')){
    tagtype <- 'SPOT'
  } else if (tagtype %in% c('miniPAT','PAT','MK10','MK10AF','psat','Xtag')){
    tagtype <- 'PSAT'
  } else if (!(tagtype %in% c('satellite','popup'))){
    stop('specified tag type is required to be either satellite or popup.')
  }

  # check dates
  if (class(dates)[1] != 'POSIXct') stop('input to dates must be of class POSIXct')

  #------------------------
  ## given a custom input
  #------------------------

  if (exists('customCols')){
    print('Using the custom columns specified in customCols argument. This is an experimental feature and is not well tested.')

    #if (is.null(fName)) stop('fName must be specified when using customColsz. This is the file name of interest.')

    #dat <- utils::read.table(paste(dir, fName, sep=''), sep=',', header=T, blank.lines.skip = F)

    #print(customCols)
    #print(dat[1,])

    warning('Defining column names using customCols as specified. These MUST exactly match observation types from the obsTypes set!')

    #names(dat) <- customCols
    #dat <- dat[which(dat$datetime != ''),]
    dat <- customCols

    #dat$datetime <- testDates(dat$datetime)
    dt.idx <- which(dat$date < dates[1] | dat$date > dates[2])
    if (length(dt.idx) > 0){
      warning('data in input dataset that is outside the bounds of specified start/end dates.')
      dat <- dat[-dt.idx,]
    }

    #dat <- dat[which(dat$argosLC != 'Z'),]
    dat <- reshape2::melt(dat, id.vars=c('DateTime'), measure.vars = names(dat)[-grep('DateTime', names(dat))])
    dat$VariableName <- dat$variable

    dat <- merge(x = dat, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
    dat <- dat[,c('DateTime','VariableID','value','VariableName','VariableUnits')]
    names(dat) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
    dat <- dat[order(dat$DateTime, dat$VariableID),]
    dat$DateTime <- as.POSIXct(dat$DateTime, tz='UTC')
    dat$DateTime <- format(dat$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
    dat <- dat[which(!is.na(dat$VariableValue)),]
    dat <- dat[which(dat$VariableValue != ' '),]

    if (exists('returnData')){
      returnData <- rbind(returnData, dat)
    } else {
      returnData <- dat
    }

  }

  #------------------------
  ## given a SPOT directory:
  #------------------------


  if (tagtype == 'SPOT' & manufacturer == 'Wildlife'){
    # use argos-locations function - the source of this would be reflected in the metadata specific to the SPOT tag
    fList <- list.files(dir, full.names = T)
    fidx <- grep('-Locations.csv', fList)
    if (length(fidx) == 0){
      print(paste('No Wildlife SPOT data to gather.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match -Locations.csv in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      argos <- utils::read.table(fList[fidx], sep=',', header=T, blank.lines.skip = F)

      # organize argos.new for flatfile format
      argos.new <- argos[which(argos$Type != 'User'),]
      nms <- tolower(names(argos.new))
      nms[grep('error.semi.major.axis', nms)] <- 'argosErrMaj'
      nms[grep('error.semi.minor.axis', nms)] <- 'argosErrMin'
      nms[grep('error.ellipse.orientation', nms)] <- 'argosErrOrient'
      nms[grep('quality', nms)] <- 'argosLC'
      names(argos.new) <- nms
      argos.new <- argos.new[which(argos.new$date != ''),]

      testDates <- function(x){
        # first try lubridate
        dt <- suppressWarnings(try(lubridate::as_datetime(x), TRUE))

        if(any(class(dt) == 'try-error') | any(is.na(dt))){
          # then try flipTime
          dt <- suppressWarnings(try(flipTime::AsDateTime(x), TRUE))

          if(any(class(dt) == 'try-error') | any(is.na(dt))){
            # attempt to switch date time to time date
            dt <- suppressWarnings(try(lubridate::parse_date_time(x, orders='HMS ymd'), TRUE))

            if(any(class(dt) == 'try-error') | any(is.na(dt))){
              # attempt to switch date time to time date
              dt <- suppressWarnings(try(lubridate::parse_date_time(x, orders='HMS dbY'), TRUE))

              if(any(class(dt) == 'try-error') | any(is.na(dt))){
                stop('Tried lubridate, flipTime and HMS ymd orders but unable to figure out datetime format.')
              }
            }
          }
        }
        return(dt)
      }

      argos.new$date <- testDates(argos.new$date)
      #argos.new$date <- as.POSIXct(argos.new$date, format='%H:%M:%S %d-%b-%Y', tz='UTC')
      argos.new <- argos.new[which(argos.new$date > dates[1] & argos.new$date < dates[2]),]
      argos.new <- argos.new[which(argos.new$argosLC != 'Z'),]
      argos.new <- reshape2::melt(argos.new, id.vars=c('date'), measure.vars = c('argosLC','argosErrMaj','argosErrMin','argosErrOrient','latitude','longitude'))
      argos.new$VariableName <- argos.new$variable

      argos.new <- merge(x = argos.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      argos.new <- argos.new[,c('date','VariableID','value','VariableName','VariableUnits')]
      names(argos.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      argos.new <- argos.new[order(argos.new$DateTime, argos.new$VariableID),]
      argos.new$DateTime <- as.POSIXct(argos.new$DateTime, tz='UTC')
      argos.new$DateTime <- format(argos.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
      argos.new <- argos.new[which(!is.na(argos.new$VariableValue)),]
      argos.new <- argos.new[which(argos.new$VariableValue != ' '),]

      if (exists('returnData')){
        returnData <- rbind(returnData, argos.new)
      } else {
        returnData <- argos.new
      }
    }

    if (exists('fe')) rm(fe)
  }

  #------------------------
  ## given a PSAT directory:
  #------------------------

  #--------------------------
  ## MTI PSAT - time series data
  #--------------------------

  if (tagtype == 'PSAT' & manufacturer == 'Microwave'){
    print('Reading Microwave PSAT for vertical data...')

    if (is.null(fName)) stop('fName of target XLS file must be specified if manufacturer is Microwave.')

    fList <- list.files(dir, full.names = T)
    fidx <- grep(fName, fList)
    if (length(fidx) == 0){
      print(paste('No Microwave data to gather using', fName, '.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match', fName, 'in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      print(paste('Reading time series data from', fName, '...'))

      depth <- gdata::read.xls(fList[fidx], sheet='Press Data', skip=1, header=T)[,1:5] # 5 cols
      temp <- gdata::read.xls(fList[fidx], sheet='Temp Data', skip=1, header=T)[,1:5] # 5 cols

      depth$Date <- as.POSIXct(depth$Date.Time, format='%m/%d/%y %H:%M', tz='UTC')
      depth$Depth <- depth$Depth.m. * -1
      temp$Date <- as.POSIXct(temp$Date.Time, format='%m/%d/%y %H:%M', tz='UTC')

      mti <- merge(depth, temp, by='Date')
      mti <- mti[which(mti$Date >= dates[1] & mti$Date <= dates[2]),]
      names(mti)[1] <- 'DateTime'
      names(mti)[3] <- 'pressure'
      names(mti)[6] <- 'depthDelta'
      names(mti)[7] <- 'depth'
      names(mti)[10] <- 'temperature'
      names(mti)[11] <- 'tempDelta'
      mti$depthDelta <- abs(as.numeric(as.character(mti$depthDelta)))
      mti$tempDelta <- abs(as.numeric(as.character(mti$tempDelta)))

      # summarize with melt
      mti.new <- reshape2::melt(mti, id.vars=c('DateTime'), measure.vars = c('temperature','depth','pressure','depthDelta','tempDelta'))
      mti.new$VariableName <- mti.new$variable

      # merge with obs types and do some formatting
      mti.new <- merge(x = mti.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      mti.new <- mti.new[,c('DateTime','VariableID','value','VariableName','VariableUnits')]
      names(mti.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      mti.new <- mti.new[order(mti.new$DateTime, mti.new$VariableID),]
      #mti.new <- mti.new[which(!is.na(mti.new$VariableValue)),]
      mti.new$DateTime <- as.POSIXct(mti.new$DateTime, tz='UTC')
      mti.new$DateTime <- format(mti.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      # convert to positive depth values
      mti.new$VariableValue[which(mti.new$VariableName=='depth')] <- abs(mti.new$VariableValue[which(mti.new$VariableName=='depth')])
      mti.new <- mti.new %>% filter(!is.na(VariableValue))

      if (exists('returnData')){
        returnData <- rbind(returnData, mti.new)
      } else {
        returnData <- mti.new
      }
    } # end fe
  } # end if tagtype
  if (exists('fe')) rm(fe)

  #--------------------------
  ## MTI PSAT - sunrise/sunset data
  #--------------------------

  if (tagtype == 'PSAT' & manufacturer == 'Microwave'){
    #print('Reading Microwave PSAT for sunrise/sunset data...')

    if (is.null(fName)) stop('fName of target XLS file must be specified if manufacturer is Microwave.')

    fList <- list.files(dir, full.names = T)
    fidx <- grep(fName, fList)
    if (length(fidx) == 0){
      print(paste('No Microwave data to gather using', fName, '.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match', fName, 'in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      print(paste('Reading light data from', fName, '...'))

      light <- gdata::read.xls(fList[fidx], sheet='Sunrise and Sunset Times', skip=1, header=T)[,1:5]
      light$Date <- as.Date(light$Date, format='%b %d, %Y', tz='UTC')
      names(light)[3] <- 'depthSunrise'
      names(light)[5] <- 'depthSunset'
      sr <- reshape2::melt(light, id.vars=c('Date', 'Sunrise.Time'), measure.vars = c('depthSunrise'))
      sr$DateTime <- as.POSIXct(paste(sr$Date, sr$Sunrise.Time), format='%Y-%m-%d %H:%M:%S', tz='UTC')
      ss <- reshape2::melt(light, id.vars=c('Date', 'Sunset.Time'), measure.vars = c('depthSunset'))
      ss$DateTime <- as.POSIXct(paste(ss$Date, ss$Sunset.Time), format='%Y-%m-%d %H:%M:%S', tz='UTC')
      light <- rbind(sr[,c('DateTime','variable','value')], ss[,c('DateTime','variable','value')])
      light$VariableName <- light$variable

      # merge with obs types and do some formatting
      mti.new <- merge(x = light, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      mti.new <- mti.new[,c('DateTime','VariableID','value','VariableName','VariableUnits')]
      names(mti.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      mti.new <- mti.new[order(mti.new$DateTime, mti.new$VariableID),]
      #mti.new <- mti.new[which(!is.na(mti.new$VariableValue)),]
      mti.new$DateTime <- as.POSIXct(mti.new$DateTime, tz='UTC')
      mti.new$DateTime <- format(mti.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      # convert to positive depth values
      mti.new$VariableValue <- abs(mti.new$VariableValue)
      mti.new <- mti.new %>% filter(!is.na(VariableValue))

      if (exists('returnData')){
        returnData <- rbind(returnData, mti.new)
      } else {
        returnData <- mti.new
      }
    } # end fe
  } # end if tagtype
  if (exists('fe')) rm(fe)

  #--------------------------
  ## MTI PSAT - location estimate data
  #--------------------------

  if (tagtype == 'PSAT' & manufacturer == 'Microwave'){
    #print('Reading Microwave PSAT for location data...')

    if (is.null(fName)) stop('fName of target XLS file must be specified if manufacturer is Microwave.')

    fList <- list.files(dir, full.names = T)
    fidx <- grep(fName, fList)
    if (length(fidx) == 0){
      print(paste('No Microwave data to gather using', fName, '.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match', fName, 'in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      print(paste('Reading location data from', fName, '...'))

      locs <- gdata::read.xls(fList[fidx], sheet='Lat&Long', skip=1, header=T, stringsAsFactors=F)#[,1:6] # 6 cols
      # dat = MWTextract(tagID = 57508, xlsfile, delta = T, minmax = F)
      day0 = as.POSIXct(locs[3,8], format='%b %d, %Y', tz='UTC') ## tag date
      x0 = as.numeric(locs[5,8:9])#(read_excel(xlsfile, skip = 5, sheet = 'Lat&Long', n_max = 1))[,8:9])
      x0[2] <- x0[2] * -1
      dayT = as.POSIXct(locs[9,8], format='%b %d, %Y', tz='UTC') ## end date
      xT = as.numeric(locs[11,8:9])
      xT[2] <- xT[2] * -1

      ## check for lon values in east or west
      if(any(!is.na(stringr::str_locate(names(locs)[3], 'W')))) is_west <- TRUE

      locs <- locs[,1:3]
      locs$Date <- as.POSIXct(locs$Date, format='%b %d, %Y', tz='UTC')
      names(locs)[1:3] <- c('DateTime','latitude','longitude')

      if (is_west) locs$longitude <- locs$longitude * -1

      # summarize with melt
      mti.new <- reshape2::melt(locs, id.vars=c('DateTime'), measure.vars = c('latitude','longitude'))
      mti.new$VariableName <- mti.new$variable

      # merge with obs types and do some formatting
      mti.new <- merge(x = mti.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      mti.new <- mti.new[,c('DateTime','VariableID','value','VariableName','VariableUnits')]
      names(mti.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      mti.new <- mti.new[order(mti.new$DateTime, mti.new$VariableID),]
      #mti.new <- mti.new[which(!is.na(mti.new$VariableValue)),]
      mti.new$DateTime <- as.POSIXct(mti.new$DateTime, tz='UTC')
      mti.new$DateTime <- format(mti.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      mti.new <- mti.new %>% filter(!is.na(VariableValue))

      if (exists('returnData')){
        returnData <- rbind(returnData, mti.new)
      } else {
        returnData <- mti.new
      }
    } # end fe
  } # end if tagtype
  if (exists('fe')) rm(fe)

  #--------------------------
  ## MTI PSAT - time series data
  #--------------------------

  if (tagtype == 'PSAT' & manufacturer == 'Microwave'){
    #print('Reading Microwave PSAT for vertical data...')

    if (is.null(fName)) stop('fName of target XLS file must be specified if manufacturer is Microwave.')

    fList <- list.files(dir, full.names = T)
    fidx <- grep(fName, fList)
    if (length(fidx) == 0){
      print(paste('No Microwave data to gather using', fName, '.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match', fName, 'in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      print(paste('Reading min max statistics from', fName, '...'))

      depth_mm <- gdata::read.xls(fList[fidx], sheet='Press Data (MinMax)', skip=1, header=T)[,1:5] # 5 cols
      temp_mm <- gdata::read.xls(fList[fidx], sheet='Temp Data (MinMax)', skip=1, header=T)[,1:5] # 5 cols
      light_mm <- gdata::read.xls(fList[fidx], sheet='Light Data (MinMax)', skip=1, header=T)[,1:3] # 5 cols


      depth_mm$Date <- as.POSIXct(depth_mm$Date.Time, format='%m/%d/%y', tz='UTC')
      temp_mm$Date <- as.POSIXct(temp_mm$Date.Time, format='%m/%d/%y', tz='UTC')
      light_mm$Date <- as.POSIXct(light_mm$Date.Time, format='%m/%d/%y', tz='UTC')

      ## merge
      mti <- merge(depth_mm, temp_mm, by='Date')
      mti <- merge(mti, light_mm, by='Date')
      mti <- mti[which(mti$Date >= dates[1] & mti$Date <= dates[2]),]
      names(mti)[1] <- 'DateTime'
      names(mti)[5] <- 'depthMin'
      names(mti)[6] <- 'depthMax'
      names(mti)[10] <- 'tempMin'
      names(mti)[11] <- 'tempMax'
      names(mti)[13] <- 'lightMin'
      names(mti)[14] <- 'lightMax'
      mti$depthMin <- abs(mti$depthMin)
      mti$depthMax <- abs(mti$depthMax)

      # summarize with melt
      mti.new <- reshape2::melt(mti, id.vars=c('DateTime'), measure.vars = c('depthMin','depthMax','tempMin','tempMax','lightMin','lightMax'))
      mti.new$VariableName <- mti.new$variable

      # merge with obs types and do some formatting
      mti.new <- merge(x = mti.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      mti.new <- mti.new[,c('DateTime','VariableID','value','VariableName','VariableUnits')]
      names(mti.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      mti.new <- mti.new[order(mti.new$DateTime, mti.new$VariableID),]
      #mti.new <- mti.new[which(!is.na(mti.new$VariableValue)),]
      mti.new$DateTime <- as.POSIXct(mti.new$DateTime, tz='UTC')
      mti.new$DateTime <- format(mti.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      mti.new <- mti.new %>% filter(!is.na(VariableValue))

      if (exists('returnData')){
        returnData <- rbind(returnData, mti.new)
      } else {
        returnData <- mti.new
      }
    } # end fe
  } # end if tagtype
  if (exists('fe')) rm(fe)

  #--------------------------
  ## LOTEK PSAT - time series data
  #--------------------------

  if (tagtype == 'PSAT' & manufacturer == 'Lotek'){
    print('Reading Lotek PSAT for vertical data...')

    if (is.null(fName)) stop('fName must be specified if manufacturer is Lotek')

    fList <- list.files(dir, full.names = T)
    fidx <- grep(fName, fList)
    if (length(fidx) == 0){
      print(paste('No Lotek data to gather using', fName, '.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match', fName, 'in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      lotek <- data.frame(readRDS('~/work/RData/FurukawaS/RegularLog_YT20070605_D2070.RDS'))
      names(lotek) <- c('DateTime', 'depth','temperature','light')
      lotek$DateTime <- as.POSIXct(lotek$DateTime, format=findDateFormat(lotek$DateTime), tz='UTC')
      lotek <- lotek[which(lotek$DateTime >= dates[1] & lotek$DateTime <= dates[2]),]

      # summarize with melt
      lotek.new <- reshape2::melt(lotek, id.vars=c('DateTime'), measure.vars = c('temperature','depth','light'))
      lotek.new$VariableName <- lotek.new$variable

      # merge with obs types and do some formatting
      lotek.new <- merge(x = lotek.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      lotek.new <- lotek.new[,c('DateTime','VariableID','value','VariableName','VariableUnits')]
      names(lotek.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      lotek.new <- lotek.new[order(lotek.new$DateTime, lotek.new$VariableID),]
      #lotek.new <- lotek.new[which(!is.na(lotek.new$VariableValue)),]
      lotek.new$DateTime <- as.POSIXct(lotek.new$DateTime, tz='UTC')
      lotek.new$DateTime <- format(lotek.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      if (exists('returnData')){
        returnData <- rbind(returnData, lotek.new)
      } else {
        returnData <- lotek.new
      }
    } # end fe
  } # end if tagtype
  if (exists('fe')) rm(fe)

  #--------------------------
  ## LOTEK PSAT - raw position data
  #--------------------------

  if (tagtype == 'PSAT' & manufacturer == 'Lotek'){
    print('Reading Lotek PSAT for position data...')

    if (is.null(fName)) stop('fName must be specified if manufacturer is Lotek')

    fList <- list.files(dir, full.names = T)
    fidx <- grep(fName, fList)
    if (length(fidx) == 0){
      print(paste('No Lotek data to gather using', fName, '.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match', fName, 'in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      lotek <- data.frame(readRDS('~/work/RData/FurukawaS/DayLog_YT20070605_D2070.RDS'))
      names(lotek) <- c('DateTime', 'longitude','latitude')
      lotek$DateTime <- as.POSIXct(lotek$DateTime, format=findDateFormat(lotek$DateTime), tz='UTC')
      lotek <- lotek[which(lotek$DateTime > dates[1] & lotek$DateTime < dates[2]),]

      # summarize with melt
      lotek.new <- reshape2::melt(lotek, id.vars=c('DateTime'), measure.vars = c('longitude','latitude'))
      lotek.new$VariableName <- lotek.new$variable

      # merge with obs types and do some formatting
      lotek.new <- merge(x = lotek.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      lotek.new <- lotek.new[,c('DateTime','VariableID','value','VariableName','VariableUnits')]
      names(lotek.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      lotek.new <- lotek.new[order(lotek.new$DateTime, lotek.new$VariableID),]
      #lotek.new <- lotek.new[which(!is.na(lotek.new$VariableValue)),]
      lotek.new$DateTime <- as.POSIXct(lotek.new$DateTime, tz='UTC')
      lotek.new$DateTime <- format(lotek.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      if (exists('returnData')){
        returnData <- rbind(returnData, lotek.new)
      } else {
        returnData <- lotek.new
      }
    } # end fe
  } # end if tagtype
  if (exists('fe')) rm(fe)


  #--------------------------
  ## WILDLIFE COMPUTERS PSAT
  #--------------------------

  if (tagtype == 'PSAT' & manufacturer == 'Wildlife'){
    print('Reading Wildlife Computers PSAT...')

    #--------------------------
    ## WC PDT - depth temp profile data
    #--------------------------

    fList <- list.files(dir, full.names = T)
    fidx <- grep('-PDTs.csv', fList)
    if (length(fidx) == 0){
      print('No PDT data to gather.')
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match -PDTs.csv in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      print('Getting PDT data...')

      pdt <- HMMoce::read.wc(filename = fList[fidx], type = 'pdt', tag = dates[1], pop = dates[2])$data

      # organize pdt.new for flatfile format
      pdt.new <- pdt
      dtime <- difftime(pdt$Date[2:nrow(pdt)], pdt$Date[1:(nrow(pdt) - 1)], units='hours')
      pdt.new$summaryPeriod <- Mode(dtime[dtime != 0])
      nms <- names(pdt.new)
      nms[grep('MinTemp', nms)] <- 'TempMin'
      nms[grep('MaxTemp', nms)] <- 'TempMax'
      names(pdt.new) <- nms
      pdt.new <- reshape2::melt(pdt.new, id.vars=c('Date','BinNum'), measure.vars = c('Depth','TempMin','TempMax','summaryPeriod'))

      binchar <- pdt.new$BinNum
      for (i in 1:length(binchar)) if (nchar(binchar[i]) < 2) binchar[i] <- paste('0', binchar[i], sep='')
      pdt.new$VariableName <- paste('Pdt', pdt.new$variable, binchar, sep='')
      pdt.new <- pdt.new[-grep('Pdtsumm', pdt.new$VariableName),]

      pdt.new <- merge(x = pdt.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      pdt.new <- pdt.new[,c('Date','VariableID','value','VariableName','VariableUnits')]
      names(pdt.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      pdt.new <- pdt.new[order(pdt.new$DateTime, pdt.new$VariableID),]
      pdt.new$DateTime <- as.POSIXct(pdt.new$DateTime, tz='UTC')
      pdt.new$DateTime <- format(pdt.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
      pdt.new <- pdt.new[which(!is.na(pdt.new$VariableID)),]

      if (exists('returnData')){
        returnData <- rbind(returnData, pdt.new)
      } else {
        returnData <- pdt.new
      }
    } # end fe
    if (exists('fe')) rm(fe)



    #--------------------------
    ## WC ARCHIVAL - depth, temperature, light
    #--------------------------

    fList <- list.files(dir, full.names = T)
    fidx <- grep('-Archive.csv', fList)
    if (length(fidx) == 0){
      print('No Archive data to read.')
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match -Archive.csv in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      print('Getting Archival data...')

      # if series exists we load it
      arch <- data.frame(data.table::fread(fList[fidx], sep=',', header = T, stringsAsFactors = F, fill=TRUE))#, skip = skipLines)
      #arch <- utils::read.table(fList[fidx], sep=',', header=T, blank.lines.skip = F, stringsAsFactors = F)#, skip = skipLines)

      nms <- names(arch)
      ## for temp start with exact "temperature" then "external.temperature" then "recorder.temp"
      temp_col <- grep('^Temperature$', nms)
      if (length(temp_col) != 1){
        temp_col <- grep('^External.Temperature$', nms)
        if (length(temp_col) != 1){
          temp_col <- grep('^Recorder.Temp$', nms)
        }
      }

      ## for depth use exact "Depth"
      depth_col <- grep('^Depth$', nms)

      ## for light use "Light.level"
      light_col <- grep('^Light.Level$', nms)

      ## for light use "Time"
      time_col <- grep('^Time$', nms)

      # organize arch.new for flatfile format
      #idx <- c(grep('time', names(arch), ignore.case = T),
      #         grep('depth', names(arch), ignore.case = T),
      #         grep('temp', names(arch), ignore.case = T),
      #         grep('light', names(arch), ignore.case = T))
      #drop_idx <- c(grep('one', names(arch), ignore.case = T),
      #              grep('internal', names(arch), ignore.case = T),
      #              grep('smooth', names(arch), ignore.case = T))
      #idx <- idx[-which(idx %in% drop_idx)]
      arch <- arch[,c(time_col, depth_col, temp_col, light_col)]
      names(arch) <- c('DateTime','depth','temperature','light')
      x <- findDateFormat(arch$DateTime[1:10])
      #arch$DateTime <- as.POSIXct(arch$DateTime, format=x, tz='UTC')
      arch$DateTime <- lubridate::parse_date_time(arch$DateTime, orders=x, tz='UTC')
      arch.new <- arch[which(arch$DateTime >= dates[1] & arch$DateTime <= dates[2]),]

      #arch <- arch[which(names(arch) %in% c('Time','Depth','Temperature','Light Level'))]
      #arch.new <- subset(arch, select=-c(One.Minute.Light.Level, Smoothed.Light.Level))
      #nms <- names(arch.new)
      #nms[grep('^Depth$', nms, ignore.case = T)] <- 'depth'
      #nms[grep('Temp', nms, ignore.case = T)] <- 'temperature'
      #nms[grep('Light', nms, ignore.case = T)] <- 'light'
      #names(arch.new) <- nms
      # summarize with melt
      arch.new <- reshape2::melt(arch.new, id.vars=c('DateTime'), measure.vars = c('depth','temperature','light'))
      arch.new$VariableName <- arch.new$variable

      # merge with obs types and do some formatting
      #arch.new <- merge(x = arch.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      arch.new <- dplyr::left_join(x = arch.new, y = obsTypes[, c("VariableID","VariableName", 'VariableUnits')], by = 'VariableName')

      arch.new <- arch.new[,c('DateTime','VariableID','value','VariableName','VariableUnits')]
      names(arch.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      arch.new <- arch.new[order(arch.new$DateTime, arch.new$VariableID),]
      arch.new <- arch.new[which(!is.na(arch.new$VariableValue)),]

      if (class(arch.new$DateTime[1])[1] != 'POSIXct'){
        x <- findDateFormat(arch.new$DateTime[1:10])
        #arch.new$DateTime <- as.POSIXct(arch.new$DateTime, tz='UTC')
        arch.new$DateTime <- lubridate::parse_date_time(arch.new$DateTime, orders=x, tz='UTC')
      }

      arch.new$DateTime <- format(arch.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      if (exists('returnData')){
        returnData <- rbind(returnData, arch.new)
      } else {
        returnData <- arch.new
      }
      rm(arch);  gc()#rm(arch.new);
    } # end fe
    if (exists('fe')) rm(fe)

    #--------------------------
    ## WC SERIES - depth and sometimes temperature
    #--------------------------

    fList <- list.files(dir, full.names = T)
    fidx <- grep('-Series.csv', fList)
    if (length(fidx) == 0){
      print(paste('No Wildlife Series data to gather.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match -Series.csv in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe & !exists('arch.new')){
      print('Getting Series data...')

      # if series exists we load it
      series <- utils::read.table(fList[fidx], sep=',', header=T, blank.lines.skip = F)
      series$dt <- lubridate::parse_date_time(paste(series$Day, series$Time), orders='dby HMS', tz='UTC')
      series <- series[which(series$dt >= dates[1] & series$dt <= dates[2]),]

      # organize series.new for flatfile format
      series.new <- subset(series, select=-c(DepthSensor))
      nms <- names(series.new)
      nms[grep('Depth', nms)] <- 'depth'
      nms[grep('DRange', nms)] <- 'depthDelta'
      nms[grep('Temperature', nms)] <- 'temperature'
      nms[grep('TRange', nms)] <- 'tempDelta'
      names(series.new) <- nms
      # summarize with melt
      series.new <- reshape2::melt(series.new, id.vars=c('dt'), measure.vars = c('depth','depthDelta','temperature','tempDelta'))
      series.new$VariableName <- series.new$variable

      # merge with obs types and do some formatting
      series.new <- merge(x = series.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      series.new <- series.new[,c('dt','VariableID','value','VariableName','VariableUnits')]
      names(series.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      series.new <- series.new[order(series.new$DateTime, series.new$VariableID),]
      series.new <- series.new[which(!is.na(series.new$VariableValue)),]
      series.new$DateTime <- as.POSIXct(series.new$DateTime, tz='UTC')
      series.new$DateTime <- format(series.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      if (exists('returnData')){
        returnData <- rbind(returnData, series.new)
      } else {
        returnData <- series.new
      }
    } # end fe
    if (exists('fe')) rm(fe)

    #--------------------------
    ## WC light - light, depth and time
    #--------------------------

    fList <- list.files(dir, full.names = T)
    fidx <- grep('-LightLoc.csv', fList)
    if (length(fidx) == 0){
      print(paste('No Wildlife light data to gather.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match -light.csv in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe & !exists('arch.new')){
      print('Getting light data...')

      # if light exists we load it
      light <- try(utils::read.table(fList[fidx], sep=',', header=T, blank.lines.skip = F, skip = 2), silent = TRUE)
      if (class(light) == 'try-error'){
        light <- try(utils::read.table(fList[fidx], sep=',', header=T, blank.lines.skip = F, skip = 0), silent = TRUE)
        if (class(light) == 'try-error'){
          stop('Cant read light data.')
        } else{
          light <- utils::read.table(fList[fidx], sep=',', header=T, blank.lines.skip = F, skip = 0)
        }

      } else{
        light <- utils::read.table(fList[fidx], sep=',', header=T, blank.lines.skip = F, skip = 2)

      }

      light.new <- extract.light(light)
      #light$dt <- lubridate::parse_date_time(paste(light$Day, light$Time), orders='dby HMS', tz='UTC')
      #light <- light[-which(light$Type %in% c('Begin','End')),]
      light.new <- light.new[which(light.new$Date >= dates[1] & light.new$Date <= dates[2]),]

      # organize light.new for flatfile format
      nms <- names(light.new)
      nms[grep('MinDepth', nms)] <- 'depthMin'
      nms[grep('MaxDepth', nms)] <- 'depthMax'
      nms[grep('Depth', nms, ignore.case = FALSE)] <- 'depth'
      nms[grep('LL', nms)] <- 'light'
      names(light.new) <- nms

      # summarize with melt
      light.new <- reshape2::melt(light.new, id.vars=c('Date'), measure.vars = c('depth','depthMin','depthMax','light'))
      light.new$VariableName <- light.new$variable

      # merge with obs types and do some formatting
      light.new <- merge(x = light.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      light.new <- light.new[,c('Date','VariableID','value','VariableName','VariableUnits')]
      names(light.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      light.new <- light.new[order(light.new$DateTime, light.new$VariableID),]
      light.new <- light.new[which(!is.na(light.new$VariableValue)),]
      light.new$DateTime <- as.POSIXct(light.new$DateTime, tz='UTC')
      light.new$DateTime <- format(light.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
      light.new <- light.new %>% distinct(DateTime, VariableID, .keep_all = TRUE)

      if (exists('returnData')){
        returnData <- rbind(returnData, light.new)
      } else {
        returnData <- light.new
      }
    } # end fe
    if (exists('fe')) rm(fe)

    #--------------------------
    ## WC MIN/MAX
    #--------------------------

    fList <- list.files(dir, full.names = T)
    fidx <- grep('-MinMaxDepth.csv', fList)
    if (length(fidx) == 0){
      print(paste('No Wildlife MinMaxDepth data to gather.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match -MinMaxDepth.csv in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      print('Getting min/max depth data...')

      # if file exists we load it
      mmd <- utils::read.table(fList[fidx], sep=',', header=T, blank.lines.skip = F)
      mmd$dt <- lubridate::parse_date_time(mmd$Date, orders=findDateFormat(mmd$Date), tz='UTC')
      mmd <- mmd[which(mmd$dt >= dates[1] & mmd$dt <= dates[2]),]

      # organize mmd.new for flatfile format
      #mmd.new <- subset(mmd, select=-c(DepthSensor))
      mmd.new <- mmd
      nms <- names(mmd.new)
      nms[grep('MinDepth', nms)] <- 'depthMin'
      #nms[grep('MinAccuracy', nms)] <- 'depthMinAcc'
      nms[grep('MaxDepth', nms)] <- 'depthMax'
      #nms[grep('TRange', nms)] <- 'depthMaxAcc'
      names(mmd.new) <- nms
      # summarize with melt
      mmd.new <- reshape2::melt(mmd.new, id.vars=c('dt'), measure.vars = c('depthMin','depthMax'))
      mmd.new$VariableName <- mmd.new$variable

      # merge with obs types and do some formatting
      mmd.new <- merge(x = mmd.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      mmd.new <- mmd.new[,c('dt','VariableID','value','VariableName','VariableUnits')]
      names(mmd.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      mmd.new <- mmd.new[order(mmd.new$DateTime, mmd.new$VariableID),]
      #mmd.new <- mmd.new[which(!is.na(mmd.new$VariableValue)),]
      mmd.new$DateTime <- as.POSIXct(mmd.new$DateTime, tz='UTC')
      mmd.new$DateTime <- format(mmd.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      if (exists('returnData')){
        returnData <- rbind(returnData, mmd.new)
      } else {
        returnData <- mmd.new
      }
    } # end fe
    if (exists('fe')) rm(fe)

    #------------
    ## SST

    fList <- list.files(dir, full.names = T)
    fidx <- grep('-SST.csv', fList)
    if (length(fidx) == 0){
      print(paste('No Wildlife SST data to gather.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match -SST.csv in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      print('Getting SST data...')

      # if file exists we load it
      sst <- utils::read.table(fList[fidx], sep=',', header=T, blank.lines.skip = F)
      sst$dt <- lubridate::parse_date_time(sst$Date, orders=findDateFormat(sst$Date), tz='UTC')
      sst <- sst[which(sst$dt >= dates[1] & sst$dt <= dates[2]),]

      sst.new <- parse_sst(sst, obsTypes)

      if (exists('returnData')){
        returnData <- rbind(returnData, sst.new)
      } else {
        returnData <- sst.new
      }
    } # end fe

    if (exists('fe')) rm(fe)

    #------------
    ## MIXED LAYER

    fList <- list.files(dir, full.names = T)
    fidx <- grep('-MixLayer.csv', fList)
    if (length(fidx) == 0){
      print(paste('No Wildlife MixedLayer data to gather.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match -MixedLayer.csv in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      print('Getting MixedLayer data...')

      # if file exists we load it
      ml <- utils::read.table(fList[fidx], sep=',', header=T, blank.lines.skip = F)
      ml$Date <- lubridate::parse_date_time(ml$Date, orders=findDateFormat(ml$Date), tz='UTC')
      ml <- ml[which(ml$Date >= dates[1] & ml$Date <= dates[2]),]

      # organize mmd.new for flatfile format
      #mmd.new <- subset(mmd, select=-c(DepthSensor))
      ml.new <- ml
      nms <- names(ml.new)
      nms[grep('SSTAve', nms)] <- 'sstMean'
      nms[grep('SSTmin', nms)] <- 'sstMin'
      nms[grep('SSTmax', nms)] <- 'sstMax'
      nms[grep('TempMin', nms)] <- 'tempMin'
      nms[grep('DepthMin', nms)] <- 'depthMin'
      nms[grep('DepthMax', nms)] <- 'depthMax'
      nms[grep('Hours', nms)] <- 'summaryPeriod'
      names(ml.new) <- nms
      # summarize with melt
      ml.new <- reshape2::melt(ml.new, id.vars=c('Date'), measure.vars = c('depthMin','depthMax','sstMean','sstMin','sstMax','tempMin'))
      ml.new$VariableName <- ml.new$variable

      # merge with obs types and do some formatting
      ml.new <- merge(x = ml.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      ml.new <- ml.new[,c('Date','VariableID','value','VariableName','VariableUnits')]
      names(ml.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      ml.new <- ml.new[order(ml.new$DateTime, ml.new$VariableID),]
      ml.new <- ml.new[which(!is.na(ml.new$VariableValue)),]
      ml.new$DateTime <- as.POSIXct(ml.new$DateTime, tz='UTC')
      ml.new$DateTime <- format(ml.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      if (exists('returnData')){
        returnData <- rbind(returnData, ml.new)
      } else {
        returnData <- ml.new
      }
    } # end fe
    if (exists('fe')) rm(fe)

    #------------
    ## HISTOS

    fList <- list.files(dir, full.names = T)
    fidx <- grep('-Histos.csv', fList)
    if (length(fidx) == 0){
      print(paste('No Wildlife Histos data to gather.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match -Histos.csv in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      print('Getting Histos data...')

      # if file exists we load it
      histo <- utils::read.table(fList[fidx], sep=',', header=T, blank.lines.skip = F)

      # try to read bin limits from file
      tat.lim <- histo[which(histo$HistType == 'TATLIMITS'), grep('Bin', names(histo))]
      tat.lim <- Filter(function(x)!all(is.na(x)), tat.lim)
      tad.lim <- histo[which(histo$HistType == 'TADLIMITS'), grep('Bin', names(histo))]
      tad.lim <- c(Filter(function(x)!all(is.na(x)), tad.lim))

      # if bins can not be read from file, check if they were specified in function call. if not, throw an error.
      if (all(is.na(tat.lim)) & !is.null(tatBins)){
        tat.lim <- tatBins
      } else if (all(is.na(tat.lim)) & is.null(tatBins)){
        stop('TAT bins could not be read from file and were not specified in function call. Please specify them and try again.')
      }

      if (all(is.na(tad.lim)) & !is.null(tadBins)){
        tad.lim <- tadBins
      } else if (all(is.na(tad.lim)) & is.null(tadBins)){
        stop('TAD bins could not be read from file and were not specified in function call. Please specify them and try again.')
      }

      histo <- histo[which(!is.na(histo$Sum)),]
      histo$dt <- lubridate::parse_date_time(histo$Date, orders=findDateFormat(histo$Date), tz='UTC')
      histo <- histo[which(histo$dt >= dates[1] & histo$dt <= dates[2]),]
      histo <- subset(histo, select=-c(NumBins))
      #histo <- Filter(function(x)!all(is.na(x)), histo)

      tat <- histo[which(histo$HistType == 'TAT'),]
      tat$summaryPeriod <- Mode(difftime(tat$dt[2:nrow(tat)], tat$dt[1:(nrow(tat) - 1)], units='hours'))
      nms <- names(tat)
      nms[grep('Bin1$', nms)] <- 'TimeAtTempBin01'
      nms[grep('Bin2$', nms)] <- 'TimeAtTempBin02'
      nms[grep('Bin3$', nms)] <- 'TimeAtTempBin03'
      nms[grep('Bin4$', nms)] <- 'TimeAtTempBin04'
      nms[grep('Bin5$', nms)] <- 'TimeAtTempBin05'
      nms[grep('Bin6$', nms)] <- 'TimeAtTempBin06'
      nms[grep('Bin7$', nms)] <- 'TimeAtTempBin07'
      nms[grep('Bin8$', nms)] <- 'TimeAtTempBin08'
      nms[grep('Bin9$', nms)] <- 'TimeAtTempBin09'
      nms[grep('Bin10$', nms)] <- 'TimeAtTempBin10'
      nms[grep('Bin11$', nms)] <- 'TimeAtTempBin11'
      nms[grep('Bin12$', nms)] <- 'TimeAtTempBin12'
      nms[grep('Bin13$', nms)] <- 'TimeAtTempBin13'
      nms[grep('Bin14$', nms)] <- 'TimeAtTempBin14'
      nms[grep('Bin15$', nms)] <- 'TimeAtTempBin15'
      nms[grep('Bin16$', nms)] <- 'TimeAtTempBin16'
      names(tat) <- nms
      tat <- Filter(function(x)!all(is.na(x)), tat)

      tat.new <- reshape2::melt(tat, id.vars=c('dt'),
                                measure.vars=c(grep('Bin', names(tat)), grep('summaryPeriod', names(tat))))


      tad <- histo[which(histo$HistType == 'TAD'),]
      tad$summaryPeriod <- Mode(difftime(tad$dt[2:nrow(tad)], tad$dt[1:(nrow(tad) - 1)], units='hours'))
      nms <- names(tad)
      nms[grep('Bin1$', nms)] <- 'TimeAtDepthBin01'
      nms[grep('Bin2$', nms)] <- 'TimeAtDepthBin02'
      nms[grep('Bin3$', nms)] <- 'TimeAtDepthBin03'
      nms[grep('Bin4$', nms)] <- 'TimeAtDepthBin04'
      nms[grep('Bin5$', nms)] <- 'TimeAtDepthBin05'
      nms[grep('Bin6$', nms)] <- 'TimeAtDepthBin06'
      nms[grep('Bin7$', nms)] <- 'TimeAtDepthBin07'
      nms[grep('Bin8$', nms)] <- 'TimeAtDepthBin08'
      nms[grep('Bin9$', nms)] <- 'TimeAtDepthBin09'
      nms[grep('Bin10$', nms)] <- 'TimeAtDepthBin10'
      nms[grep('Bin11$', nms)] <- 'TimeAtDepthBin11'
      nms[grep('Bin12$', nms)] <- 'TimeAtDepthBin12'
      nms[grep('Bin13$', nms)] <- 'TimeAtDepthBin13'
      nms[grep('Bin14$', nms)] <- 'TimeAtDepthBin14'
      nms[grep('Bin15$', nms)] <- 'TimeAtDepthBin15'
      nms[grep('Bin16$', nms)] <- 'TimeAtDepthBin16'
      names(tad) <- nms
      tad <- Filter(function(x)!all(is.na(x)), tad)

      tad.new <- reshape2::melt(tad, id.vars=c('dt'),
                                measure.vars=c(grep('Bin', names(tad)), grep('summaryPeriod', names(tad))))
      histo.new <- rbind(tat.new, tad.new)
      histo.new$VariableName <- histo.new$variable

      # merge with obs types and do some formatting
      histo.new <- merge(x = histo.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      histo.new <- histo.new[,c('dt','VariableID','value','VariableName','VariableUnits')]
      names(histo.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')

      # deal with TAD bin limits
      hdb <- obsTypes[grep('HistDepthBin', obsTypes$VariableName), c('VariableID', 'VariableName')]
      hdb$Value <- NA
      hdb$Value[1] <- 0
      idx <- grep('Max', hdb$VariableName)[1:length(tad.lim)]
      for (zz in idx){
        hdb$Value[zz] <- unlist(tad.lim[which(idx == zz)])
        if (which(idx == zz) != 1 & !is.na(unlist(tad.lim[which(idx == zz)]))) hdb$Value[zz - 1] <- unlist(tad.lim[which(idx == zz) - 1]) + 0.1
      }
      hdb <- hdb[which(!is.na(hdb$Value)),]

      # deal with TAT bin limits
      htb <- obsTypes[grep('HistTempBin', obsTypes$VariableName), c('VariableID', 'VariableName')]
      htb$Value <- NA
      htb$Value[1] <- 0
      idx <- grep('Max', htb$VariableName)[1:length(tat.lim)]
      for (zz in idx){
        htb$Value[zz] <- unlist(tat.lim[which(idx == zz)])
        if (which(idx == zz) != 1 & !is.na(unlist(tat.lim[which(idx == zz)]))) htb$Value[zz - 1] <- unlist(tat.lim[which(idx == zz) - 1]) + 0.1
      }
      htb <- htb[which(!is.na(htb$Value)),]

      # now duplicate each bin limit data frame for each time point in the histogram data
      #tat.dates <- unique(tat.new$dt)
      #for (zz in 1:length(tat.dates)){
      #histo.new <- rbind(histo.new, data.frame(DateTime = rep(tat.dates[zz], nrow(htb)), VariableID = htb$VariableID,
      #                                         VariableValue = htb$Value, VariableName = htb$VariableName, VariableUnits = 'Celsius'))
      histo.new <- rbind(histo.new, data.frame(DateTime = NA, VariableID = htb$VariableID,
                                               VariableValue = htb$Value, VariableName = htb$VariableName, VariableUnits = 'Celsius'))
      #}

      #tad.dates <- unique(tad.new$dt)
      #for (zz in 1:length(tad.dates)){
      histo.new <- rbind(histo.new, data.frame(DateTime = NA, VariableID = hdb$VariableID,
                                               VariableValue = hdb$Value, VariableName = hdb$VariableName, VariableUnits = 'meter'))
      #}


      histo.new <- histo.new[order(histo.new$DateTime, histo.new$VariableID),]
      histo.new <- histo.new[which(!is.na(histo.new$VariableValue)),]
      histo.new$DateTime <- as.POSIXct(histo.new$DateTime, tz='UTC')
      histo.new$DateTime <- format(histo.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      if (exists('returnData')){
        returnData <- rbind(returnData, histo.new)
      } else {
        returnData <- histo.new
      }
    }
    if (exists('fe')) rm(fe)

    #------------
    ## GPE3 - the source of these positions, in this case GPE3, would be reflected in the metadata specific to the PSAT tag as "modeled" and "GPE3"

    fList <- list.files(dir, full.names = T)
    fidx <- grep('-GPE3.csv', fList)
    if (length(fidx) == 0 | gpe3 == FALSE){
      print(paste('No Wildlife GPE3 data to gather.', sep=''))
      fe <- FALSE
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match -GPE3.csv in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      print('Getting GPE3 data...')

      #setwd(myDir)
      ncFile <- list.files(dir, full.names = T)[grep('GPE3.nc', list.files(dir, full.names = T))]
      csvFile <- list.files(dir, full.names = T)[grep('GPE3.csv',list.files(dir, full.names = T))]
      if (length(ncFile) > 1 | length(csvFile) > 1) stop('Multiple matches to .nc or GPE3.csv in the specified directory.')

      out <- getCtr_gpe3(ncFile, csvFile, threshold=5, makePlot=F)
      df <- lapply(out, FUN=function(x) cbind(x$loc, x$xDist, x$yDist))
      df <- rlist::list.rbind(df)
      names(df) <- c('ptt','date','lat','lon','xDist','yDist')

      # organize mmd.new for flatfile format
      gpe <- df
      nms <- names(gpe)
      nms[grep('lon', nms)] <- 'longitude'
      nms[grep('lat', nms)] <- 'latitude'
      nms[grep('xDist', nms)] <- 'longitudeError'
      nms[grep('yDist', nms)] <- 'latitudeError'
      names(gpe) <- nms
      # summarize with melt
      gpe <- reshape2::melt(gpe, id.vars=c('date'), measure.vars = c('longitude','latitude','longitudeError','latitudeError'))
      gpe$VariableName <- gpe$variable

      # merge with obs types and do some formatting
      gpe <- merge(x = gpe, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      gpe <- gpe[,c('date','VariableID','value','VariableName','VariableUnits')]
      names(gpe) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      gpe <- gpe[order(gpe$DateTime, gpe$VariableID),]
      gpe <- gpe[which(!is.na(gpe$VariableValue)),]
      gpe$DateTime <- as.POSIXct(gpe$DateTime, tz='UTC')
      #gpe <- gpe[which(gpe$DateTime >= dates[1] & gpe$DateTime <= dates[2]),]
      gpe$DateTime <- format(gpe$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

      if (exists('returnData')){
        returnData <- rbind(returnData, gpe)
      } else {
        returnData <- gpe
      }

    }
    if (exists('fe')) rm(fe)

  } # close WC PSAT


  #--------------------------
  ## FINISHED
  #--------------------------
  print('Cleaning up...')

  ## cleaning step to ensure no timestamp has multiple entries for the same variableid
  returnData <- distinct(returnData, DateTime, VariableName, .keep_all = TRUE)
  returnData <- returnData[order(returnData$DateTime, returnData$VariableID),]
  ## convert to char and fill NAs with blanks for dealing with TAD/TAT bins
  returnData$DateTime <- as.character(returnData$DateTime)
  returnData$DateTime[which(is.na(returnData$DateTime))] <- ''

  if(exists('write_direct')){
    if (write_direct == TRUE & exists('etuff_file')){
      ## write the output
      build_meta_head(meta_row = meta_row, filename = etuff_file, write_hdr = T)
      #write.table(etuff, file = etuff_file, sep = ',', col.names = F, row.names = F, quote = F, append=T)
      print(utils::head(returnData))
      data.table::fwrite(returnData, file = etuff_file, sep = ',', col.names = F, row.names = F, quote = F, append=T)

      print(paste('Data added to eTUFF file ', etuff_file, '.', sep=''))

    } else{
      stop('Must specify etuff_file if write_direct = TRUE.')
    }
  }

  ## output of class etuff
  print('Generating output object...')
  #df <- returnData %>% dplyr::select(-c(VariableID, VariableUnits)) %>% tidyr::spread(VariableName, VariableValue)
  df <- returnData %>% dplyr::select(-c(VariableID, VariableUnits)) %>%
    tidyfast::dt_pivot_wider(names_from = VariableName, values_from = VariableValue) %>%
    as.data.frame()

  names(df)[1] <- 'DateTime'

  print('Generating bins...')
  ## datetime is blank for histo bins and incorporates adjustments above for bins whether or not theyre provided as inputs
  if (any(df$DateTime == '')){
    bins <- df[which(df$DateTime == ''),]
    drop_idx <- which(apply(bins, 2, FUN=function(x) all(is.na(x) | x == '')))
    bins <- bins[,-drop_idx]
    df <- df[which(df$DateTime != ''),]
  }
  if (!exists('bins')) bins <- NULL

  #df$DateTime <- as.POSIXct(df$DateTime, tz='UTC')
  df$DateTime <- fasttime::fastPOSIXct(df$DateTime, tz='UTC')
  df$id <- meta_row$instrument_name

  etuff <- list(etuff = df, meta = meta_row, bins = bins)
  class(etuff) <- 'etuff'
  return(etuff)
}# end function




