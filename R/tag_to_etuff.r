#'
#' @param dir is directory the target data is stored in
#' @param manufacturer is character indicating tag manufacturer. Choices are 'Wildlife','Microwave','Lotek'.
#' @param tagtype is character. Choices are 'PSAT', 'SPOT', ...
#' @param fName is character indicating the file name of interest to read. This is currently only used for reading archival data from Microwave or Lotek tags.
#'

tag_to_etuff <- function(dir, manufacturer, tagtype, fName = NULL){

  # check the specific manufacturer is actually supported
  if (!(manufacturer %in% c('Microwave','Wildlife','Lotek'))) stop('the specified manufacturer is not supported.')

  # attempt to reconcile a poorly specified tagtype
  if (!(tagtype %in% c(''))){
    # if tagtype doesnt match what we expect, try to figure it out

    if (manufacturer == 'Wildlife'){
      # check for SPOT


      # check for PSAT

    }

    if (manufacturer == 'Microwave'){
      # check for PSAT

    }

    if (manufacturer == 'Lotek'){
      # check for archival

    }

  }

  #------------------------
  ## given a SPOT directory:
  #------------------------
  if (tagtype == 'SPOT' & manufacturer == 'Wildlife'){
    # use argos-locations function - the source of this would be reflected in the metadata specific to the SPOT tag
    fList <- list.files(dir)
    fidx <- grep('-Locations.csv', fList)
    if (length(fidx) == 0){
      break
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match -Locations.csv in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }

    if (fe){
      argos <- read.table(paste(dir, meta$ptt[i], '-Locations.csv', sep=''), sep=',', header=T, blank.lines.skip = F)

      # organize argos.new for flatfile format
      argos.new <- argos[which(argos$Type != 'User'),]
      nms <- tolower(names(argos.new))
      nms[grep('error.semi.major.axis', nms)] <- 'argosErrMaj'
      nms[grep('error.semi.minor.axis', nms)] <- 'argosErrMin'
      nms[grep('error.ellipse.orientation', nms)] <- 'argosErrOrient'
      nms[grep('quality', nms)] <- 'argosLC'
      names(argos.new) <- nms
      argos.new$date <- as.POSIXct(argos.new$date, format='%H:%M:%S %d-%b-%Y', tz='UTC')
      argos.new <- reshape2::melt(argos.new, id.vars=c('date'), measure.vars = c('argosLC','argosErrMaj','argosErrMin','argosErrOrient'))
      argos.new$VariableName <- argos.new$variable

      argos.new <- merge(x = argos.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      argos.new <- argos.new[,c('date','VariableID','value','VariableName','VariableUnits')]
      names(argos.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      argos.new <- argos.new[order(argos.new$DateTime, argos.new$VariableID),]
      argos.new$DateTime <- as.POSIXct(argos.new$DateTime, tz='UTC')
      argos.new$DateTime <- format(argos.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
      if (exists('returnData')){
        returnData <- rbind(returnData, argos.new)
      } else {
        returnData <- argos.new
      }
    }

    rm(fe)
  }

  #------------------------
  ## given a PSAT directory:
  #------------------------

  #--------------------------
  ## MTI PSAT - time series data
  #--------------------------
  if (tagtype == 'PSAT' & manufacturer == 'Microwave'){
    if (is.null(fName)) stop('fName must be specified if manufacturer is Microwave.')

    fList <- list.files(dir)
    fidx <- grep(fName, fList)
    if (length(fidx) == 0){
      break
    } else if (length(fidx) > 1){
      stop(paste(length(fidx), 'files match', fName, 'in the current directory. Ensure there are no duplicated extensions and try again.'))
    } else if (length(fidx) == 1){
      fe <- TRUE
    }
    if (fe){
      print(paste('Reading archival data sheet from', fName, '...'))
      mti <- gdata::read.xls(fName, sheet='Archival Data', skip=2,
                             colClasses = c(rep(NA,7), rep('NULL', 4)),
                             header=F)
      names(mti) <- c('DateTime', 'tempval','pressureval','lightval','temperature','depth','light')
      mti$DateTime <- as.POSIXct(mti$DateTime, format=HMMoce::findDateFormat(mti$DateTime), tz='UTC')

      # summarize with melt
      mti.new <- reshape2::melt(mti, id.vars=c('DateTime'), measure.vars = c('temperature','depth','light'))
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

      if (exists('returnData')){
        returnData <- rbind(returnData, mti.new)
      } else {
        returnData <- mti.new
      }
    }
    rm(fe)

    #--------------------------
    ## LOTEK PSAT - time series data
    #--------------------------

    fe <- file.exists(paste(dir, '-PDTs.csv', sep=''))

    if (fe){
      lotek <- data.frame(readRDS('~/work/RData/FurukawaS/RegularLog_YT20070605_D2070.RDS'))
      names(lotek) <- c('DateTime', 'depth','temperature','light')
      lotek$DateTime <- as.POSIXct(lotek$DateTime, format=HMMoce::findDateFormat(lotek$DateTime), tz='UTC')

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

    }
    rm(fe)

    #--------------------------
    ## LOTEK PSAT - raw position data
    #--------------------------

    fe <- file.exists(paste(dir, '-PDTs.csv', sep=''))

    if (fe){
      lotek <- data.frame(readRDS('~/work/RData/FurukawaS/DayLog_YT20070605_D2070.RDS'))
      names(lotek) <- c('DateTime', 'longitude','latitude')
      lotek$DateTime <- as.POSIXct(lotek$DateTime, format=HMMoce::findDateFormat(lotek$DateTime), tz='UTC')

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

    }
    rm(fe)

    #--------------------------
    ## WC PDT - depth temp profile data
    #--------------------------
    fe <- file.exists(paste(dir, '-PDTs.csv', sep=''))

    if (fe){
      pdt <- read.wc(filename = paste(myDir, ptt, '-PDTs.csv',sep=''), type = 'pdt', tag=tag, pop=pop)$data

      # organize pdt.new for flatfile format
      pdt.new <- pdt
      nms <- names(pdt.new)
      nms[grep('MinTemp', nms)] <- 'TempMin'
      nms[grep('MaxTemp', nms)] <- 'TempMax'
      names(pdt.new) <- nms
      pdt.new <- melt(pdt.new, id.vars=c('Date','BinNum'), measure.vars = c('Depth','TempMin','TempMax'))

      binchar <- pdt.new$BinNum
      for (i in 1:length(binchar)) if (nchar(binchar[i]) < 2) binchar[i] <- paste('0', binchar[i], sep='')
      pdt.new$VariableName <- paste('Pdt', pdt.new$variable, binchar, sep='')

      pdt.new <- merge(x = pdt.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      pdt.new <- pdt.new[,c('Date','VariableID','value','VariableName','VariableUnits')]
      names(pdt.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      pdt.new <- pdt.new[order(pdt.new$DateTime, pdt.new$VariableID),]
      pdt.new$DateTime <- as.POSIXct(pdt.new$DateTime, tz='UTC')
      pdt.new$DateTime <- format(pdt.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

    }
    rm(fe)

    #--------------------------
    ## WC ARCHIVAL - depth, temperature, light
    #--------------------------
    fe <- file.exists(paste(dir, '-Archive.csv', sep=''))

    if (fe){
      # if series exists we load it
      arch <- read.table(paste('~/work/Data/meso/', tolower(meta$speciescode[i]),
                               '/', meta$ptt[i], '/', meta$ptt[i], '-Series.csv', sep=''), sep=',', header=T, blank.lines.skip = F)
      arch$dt <- as.POSIXct(arch$Time, format=HMMoce::findDateFormat(arch$Time), tz='UTC')

      # organize arch.new for flatfile format
      arch.new <- subset(arch, select=-c(One.Minute.Light.Level, Smoothed.Light.Level))
      nms <- names(arch.new)
      nms[grep('Depth', nms)] <- 'depth'
      nms[grep('Temperature', nms)] <- 'temperature'
      nms[grep('Light.Level', nms)] <- 'light'
      names(arch.new) <- nms
      # summarize with melt
      arch.new <- reshape2::melt(arch.new, id.vars=c('dt'), measure.vars = c('depth','temperature','light'))
      arch.new$VariableName <- arch.new$variable

      # merge with obs types and do some formatting
      arch.new <- merge(x = arch.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      arch.new <- arch.new[,c('dt','VariableID','value','VariableName','VariableUnits')]
      names(arch.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      arch.new <- arch.new[order(arch.new$DateTime, arch.new$VariableID),]
      arch.new <- arch.new[which(!is.na(arch.new$VariableValue)),]
      arch.new$DateTime <- as.POSIXct(arch.new$DateTime, tz='UTC')
      arch.new$DateTime <- format(arch.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
    }
    rm(fe)

    #--------------------------
    ## WC SERIES - depth and sometimes temperature
    #--------------------------
    fe <- file.exists(paste(dir, '-Series.csv', sep=''))

    if (fe & !exists(arch.new)){
      # if series exists we load it
      series <- read.table(paste('~/work/Data/meso/', tolower(meta$speciescode[i]),
                                 '/', meta$ptt[i], '/', meta$ptt[i], '-Series.csv', sep=''), sep=',', header=T, blank.lines.skip = F)
      series$dt <- lubridate::parse_date_time(paste(series$Day, series$Time), orders='dby HMS', tz='UTC')

      # organize series.new for flatfile format
      series.new <- subset(series, select=-c(DepthSensor))
      nms <- names(series.new)
      nms[grep('Depth', nms)] <- 'depthMean'
      nms[grep('DRange', nms)] <- 'depthStDev'
      nms[grep('Temperature', nms)] <- 'tempMean'
      nms[grep('TRange', nms)] <- 'tempStDev'
      names(series.new) <- nms
      # summarize with melt
      series.new <- reshape2::melt(series.new, id.vars=c('dt'), measure.vars = c('depthMean','depthStDev','tempMean','tempStDev'))
      series.new$VariableName <- series.new$variable

      # merge with obs types and do some formatting
      series.new <- merge(x = series.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      series.new <- series.new[,c('dt','VariableID','value','VariableName','VariableUnits')]
      names(series.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      series.new <- series.new[order(series.new$DateTime, series.new$VariableID),]
      series.new <- series.new[which(!is.na(series.new$VariableValue)),]
      series.new$DateTime <- as.POSIXct(series.new$DateTime, tz='UTC')
      series.new$DateTime <- format(series.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
    }
    rm(fe)

    #--------------------------
    ## WC MIN/MAX
    #--------------------------
    fe <- file.exists(paste(dir, '-MinMaxDepth.csv', sep=''))

    if (fe){
      # if file exists we load it
      mmd <- read.table(paste('~/work/Data/meso/', tolower(meta$speciescode[i]),
                              '/', meta$ptt[i], '/', meta$ptt[i], '-MinMaxDepth.csv', sep=''), sep=',', header=T, blank.lines.skip = F)
      #mmd <- read.table(paste(myDir, ptt, '-MinMaxDepth.csv', sep=''), sep=',', header=T, blank.lines.skip = F)
      mmd$dt <- parse_date_time(mmd$Date, orders='HMS dbY', tz='UTC')

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
      mmd.new <- melt(mmd.new, id.vars=c('dt'), measure.vars = c('depthMin','depthMax'))
      mmd.new$VariableName <- mmd.new$variable

      # merge with obs types and do some formatting
      mmd.new <- merge(x = mmd.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      mmd.new <- mmd.new[,c('dt','VariableID','value','VariableName','VariableUnits')]
      names(mmd.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      mmd.new <- mmd.new[order(mmd.new$DateTime, mmd.new$VariableID),]
      #mmd.new <- mmd.new[which(!is.na(mmd.new$VariableValue)),]
      mmd.new$DateTime <- as.POSIXct(mmd.new$DateTime, tz='UTC')
      mmd.new$DateTime <- format(mmd.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
    }
    rm(fe)

    #------------
    ## SST
    ## for WC mP's: if SST source = MixLayer then the value is an SST summary (mean?) integrated over the summary period
    ##              if SST source = TimeSeries then its discrete SST measurement with timestamp however depth may not be full resolution depending on how it was encoded in the source
    ##              if SST source = LightLoc then discrete SST measurements and depth at full resolution
    ##              if SST source = Status then unknown?? ask WC
    fe <- file.exists(paste(dir, '-SST.csv', sep=''))

    if (fe){
      # if file exists we load it
      sst <- read.table(paste('~/work/Data/meso/', tolower(meta$speciescode[i]),
                              '/', meta$ptt[i], '/', meta$ptt[i], '-SST.csv', sep=''), sep=',', header=T, blank.lines.skip = F)
      #sst <- read.table(paste(myDir, ptt, '-SST.csv', sep=''), sep=',', header=T, blank.lines.skip = F)

      sst$dt <- parse_date_time(sst$Date, orders='HMS dbY', tz='UTC')
      # organize mmd.new for flatfile format
      #mmd.new <- subset(mmd, select=-c(DepthSensor))
      sst.new <- sst
      nms <- names(sst.new)
      nms[grep('MinDepth', nms)] <- 'depthMin'
      #nms[grep('MinAccuracy', nms)] <- 'depthMinAcc'
      nms[grep('MaxDepth', nms)] <- 'depthMax'
      #nms[grep('TRange', nms)] <- 'depthMaxAcc'
      names(sst.new) <- nms
      # summarize with melt
      sst.new <- melt(sst.new, id.vars=c('dt'), measure.vars = c('depthMin','depthMax'))
      sst.new$VariableName <- sst.new$variable

      # merge with obs types and do some formatting
      sst.new <- merge(x = sst.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      sst.new <- sst.new[,c('dt','VariableID','value','VariableName','VariableUnits')]
      names(sst.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      sst.new <- sst.new[order(sst.new$DateTime, sst.new$VariableID),]
      #sst.new <- sst.new[which(!is.na(sst.new$VariableValue)),]
      sst.new$DateTime <- as.POSIXct(sst.new$DateTime, tz='UTC')
      sst.new$DateTime <- format(sst.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

    }

    rm(fe)

    #------------
    ## MIXED LAYER
    fe <- file.exists(paste(dir, '-ml.csv', sep=''))

    if (fe){
      # if file exists we load it
      ml <- read.table(paste('~/work/Data/meso/', tolower(meta$speciescode[i]),
                             '/', meta$ptt[i], '/', meta$ptt[i], '-ml.csv', sep=''), sep=',', header=T, blank.lines.skip = F)
      #ml <- read.table(paste(myDir, ptt, '-MixLayer.csv', sep=''), sep=',', header=T, blank.lines.skip = F)

      ml$Date <- parse_date_time(ml$Date, orders='HMS dbY', tz='UTC')
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
      names(ml.new) <- nms
      # summarize with melt
      ml.new <- melt(ml.new, id.vars=c('Date'), measure.vars = c('depthMin','depthMax','sstMean','sstMin','sstMax','tempMin'))
      ml.new$VariableName <- ml.new$variable

      # merge with obs types and do some formatting
      ml.new <- merge(x = ml.new, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      ml.new <- ml.new[,c('Date','VariableID','value','VariableName','VariableUnits')]
      names(ml.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      ml.new <- ml.new[order(ml.new$DateTime, ml.new$VariableID),]
      ml.new <- ml.new[which(!is.na(ml.new$VariableValue)),]
      ml.new$DateTime <- as.POSIXct(ml.new$DateTime, tz='UTC')
      ml.new$DateTime <- format(ml.new$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

    }

    rm(fe)

    #------------
    ## HISTOS
    fe <- file.exists(paste(dir, '-Histos.csv', sep=''))

    tadBins.i <- tadBins[which(tadBins$SpeciesCode %in% meta$speciescode[i] & tadBins$PTT %in% meta$ptt[i]),]
    tatBins.i <- tatBins[which(tatBins$SpeciesCode %in% meta$speciescode[i] & tatBins$PTT %in% meta$ptt[i]),]

    if (fe){
      # if file exists we load it
      histo <- read.table(paste('~/work/Data/meso/', tolower(meta$speciescode[i]),
                                '/', meta$ptt[i], '/', meta$ptt[i], '-Histos.csv', sep=''), sep=',', header=T, blank.lines.skip = F)
      histo <- histo[which(!is.na(histo$Sum)),]
      histo$dt <- parse_date_time(histo$Date, orders='HMS dbY', tz='UTC')
      histo$day <- as.Date(histo$dt, tz='UTC')
      histo <- histo[which(histo$day >= as.Date(min(track$dt)) & histo$day <= as.Date(max(track$dt))),]
      names(histo) <- tolower(names(histo))
      histo <- histo[,tolower(c('Ptt','dt','HistType','NumBins','Sum','Bin1','Bin2','Bin3','Bin4','Bin5','Bin6','Bin7','Bin8','Bin9','Bin10','Bin11','Bin12','Bin13','Bin14'))]
      histo.m <- reshape2::melt(histo, id.vars=tolower(c('Ptt','dt','HistType','NumBins','Sum')),
                                measure.vars=6:19)

      if (any(as.numeric(difftime(histo.m$dt[2:nrow(histo.m)], histo.m$dt[1:(nrow(histo.m)-1)], units='secs')) < (24*3600))){
        histo.m$dates <- as.Date(histo.m$dt)
        histo.new <- data.frame(histo.m %>% group_by(ptt, dates, histtype, variable) %>% summarise(meanval=mean(value, na.rm=T)))
        histo.new$dates <- as.POSIXct(histo.new$dates, tz='UTC')

      } else{
        histo.new <- histo.m[,c('ptt','dt','histtype','variable','value')]
        names(histo.new) <- c('ptt','dates','histtype','variable','meanval')
      }
      rm(histo.m)

      histo.new <- histo.new[order(histo.new$histtype, histo.new$dates, histo.new$variable),]
      histo.new$binmeta <- NA
      ubins <- unique(histo.new$variable[which(histo.new$histtype == 'TAT')])
      for (zz in 1:length(ubins)) histo.new$binmeta[which(histo.new$variable %in% ubins[zz] & histo.new$histtype == 'TAT')] <- tatBins.i[,which(names(tatBins.i) %in% ubins[zz])]

      ubins <- unique(histo.new$variable[which(histo.new$histtype == 'TAD')])
      for (zz in 1:length(ubins)) histo.new$binmeta[which(histo.new$variable %in% ubins[zz] & histo.new$histtype == 'TAD')] <- tadBins.i[,which(names(tadBins.i) %in% ubins[zz])]
      histos <- histo.new
      meta$histos[i] <- T

      # get rid of wonky depth sensor
      if (meta$ptt[i] == 100976) histos$meanval[which(histos$binmeta > 1500)] <- NA

    } else{
      histos <- NULL
      meta$histos[i] <- F

    }

    #------------
    ## GPE3 - the source of these positions, in this case GPE3, would be reflected in the metadata specific to the PSAT tag as "modeled" and "GPE3"

    fe <- file.exists(paste(dir, '-Histos.csv', sep=''))

    if (fe){
      setwd(myDir)
      ncFile <- list.files()[grep('.nc', list.files())]
      csvFile <- list.files()[grep('GPE3.csv',list.files())]

      out <- getCtr_gpe3(ncFile, csvFile, threshold=50, makePlot=F)
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
      gpe <- melt(gpe, id.vars=c('date'), measure.vars = c('longitude','latitude','longitudeError','latitudeError'))
      gpe$VariableName <- gpe$variable

      # merge with obs types and do some formatting
      gpe <- merge(x = gpe, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
      gpe <- gpe[,c('date','VariableID','value','VariableName','VariableUnits')]
      names(gpe) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
      gpe <- gpe[order(gpe$DateTime, gpe$VariableID),]
      gpe <- gpe[which(!is.na(gpe$VariableValue)),]
      gpe$DateTime <- as.POSIXct(gpe$DateTime, tz='UTC')
      gpe$DateTime <- format(gpe$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss

    }
    rm(fe)



    ## MIX LAYER?


    # use lightloc function (skip for now?)


  }
