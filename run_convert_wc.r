
## changes to ObservationTypes
# 1) need to account for accuracy of depth/temp measurements (at lesat by WC). this isn't the same as the current stdev metric
# 2) comment that WC often has mixed layer metrics although perhaps not worth adding to obs types?
# 3) sstDateTime as a VARIABLE necessary? why not just timestamp the sst variable like normal?
# 4) how to deal with WC light data?



## after formulating flatfile:
# check for rows that are completely duplicated (with or without the same VariableValue)
# check for outliers in variablevalue
# cut based on start/end timestamps

## questions for WC:
# way to recover histos bins if you dont have config file (pre-portal config)
# how does status source work in -SST


# give the function a row of metadata and a directory
## the function gets necessary info from meta like tag type, start/end, etc.
## then runs appropriate parse functions on the necessary files in the directory

metaTypes <- read.csv(url("https://raw.githubusercontent.com/tagbase/tagbase/master/eTagMetadataInventory.csv"))

tFile <- tempfile()
curl::curl_download(url="https://github.com/tagbase/tagbase/blob/master/eTUFF-ObservationTypes.xlsx?raw=true", destfile = tFile)
obsTypes <- gdata::read.xls(tFile, sheet='ObservationTypes')

# identify appropriate meta row (?)

# use meta function to build the header

#------------------------
## given a PSAT directory:
#------------------------

# use lightloc function (skip for now?)

# GPE3 - the source of this would be reflected in the metadata specific to the PSAT tag

#------------------------
## given a SPOT directory:
#------------------------

# use argos-locations function - the source of this would be reflected in the metadata specific to the SPOT tag









tFile <- tempfile()
curl::curl_download(url="https://github.com/tagbase/tagbase/blob/master/eTUFF-ObservationTypes.xlsx?raw=true", destfile = tFile)
obsTypes <- gdata::read.xls(tFile, sheet='ObservationTypes')

#--------------------------
## WC PDT - depth temp profile data
#--------------------------
pdt <- read.wc(filename = paste(myDir, ptt, '-PDTs.csv',sep=''), type = 'pdt', tag=tag, pop=pop)$data
nms <- names(pdt)
nms[grep('MinTemp', nms)] <- 'TempMin'
nms[grep('MaxTemp', nms)] <- 'TempMax'
names(pdt) <- nms
pdt <- melt(pdt, id.vars=c('Date','BinNum'), measure.vars = c('Depth','TempMin','TempMax'))

binchar <- pdt$BinNum
for (i in 1:length(binchar)) if (nchar(binchar[i]) < 2) binchar[i] <- paste('0', binchar[i], sep='')
pdt$VariableName <- paste('Pdt', pdt$variable, binchar, sep='')

pdt.new <- merge(x = pdt, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
pdt.new <- pdt.new[,c('Date','VariableID','value','VariableName','VariableUnits')]
names(pdt.new) <- c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')
pdt.new <- pdt.new[order(pdt.new$DateTime, pdt.new$VariableID),]

#--------------------------
## WC SERIES
#--------------------------
# if series exists we load it
series <- read.table(paste(myDir, ptt, '-Series.csv', sep=''), sep=',', header=T, blank.lines.skip = F)
series <- getSeriesTemp(series, pdt, results, flag = T)

series$dt <- parse_date_time(paste(series$day, series$time), orders='dby HMS', tz='UTC')
#series <- series[,c(2,19,16,11:13)]
series <- series[,c('ptt','dt','doy','depth','temperature')]


#=============================
#=============================

## get these from meta header...
# TAG/POPUP DATES AND LOCATIONS (dd, mm, YYYY, lat, lon)
tagyr <- lubridate::year(meta$tagdate[i])
iniloc <- data.frame(matrix(c(day(meta$tagdate[i]), month(meta$tagdate[i]), year(meta$tagdate[i]), meta$taglat[i], meta$taglon[i],
                              day(meta$popdate[i]), month(meta$popdate[i]), year(meta$popdate[i]), meta$poplat[i], meta$poplon[i]), nrow = 2, ncol = 5, byrow = T))
colnames(iniloc) = list('day','month','year','lat','lon')
tag <- as.POSIXct(paste(iniloc[1,1], '/', iniloc[1,2], '/', iniloc[1,3], sep=''), format = '%d/%m/%Y', tz='UTC')
pop <- as.POSIXct(paste(iniloc[2,1], '/', iniloc[2,2], '/', iniloc[2,3], sep=''), format = '%d/%m/%Y', tz='UTC')
# VECTOR OF DATES FROM DATA. THIS WILL BE THE TIME STEPS, T, IN THE LIKELIHOODS
dateVec <- as.Date(seq(tag, pop, by = 'day'))

#--------------------------
## WC PDT - depth temp profile data
#--------------------------
fe <- file.exists(paste('~/work/Data/meso/', tolower(meta$speciescode[i]), '/', meta$ptt[i], '/', meta$ptt[i], '-PDTs.csv', sep=''))

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
## WC SERIES - depth and sometimes temperature
#--------------------------

fe <- file.exists(paste('~/work/Data/meso/', tolower(meta$speciescode[i]), '/', meta$ptt[i], '/', meta$ptt[i], '-Series.csv', sep=''))

if (fe){
  # if series exists we load it
  series <- read.table(paste('~/work/Data/meso/', tolower(meta$speciescode[i]),
                             '/', meta$ptt[i], '/', meta$ptt[i], '-Series.csv', sep=''), sep=',', header=T, blank.lines.skip = F)
  #series$dt <- parse_date_time(paste(series$Day, series$Time), orders='dby HMS', tz='UTC')

  # organize series.new for flatfile format
  series.new <- subset(series, select=-c(DepthSensor))
  nms <- names(series.new)
  nms[grep('Depth', nms)] <- 'depthMean'
  nms[grep('DRange', nms)] <- 'depthStDev'
  nms[grep('Temperature', nms)] <- 'tempMean'
  nms[grep('TRange', nms)] <- 'tempStDev'
  names(series.new) <- nms
  # summarize with melt
  series.new <- melt(series.new, id.vars=c('dt'), measure.vars = c('depthMean','depthStDev','tempMean','tempStDev'))
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
fe <- file.exists(paste('~/work/Data/meso/', tolower(meta$speciescode[i]), '/', meta$ptt[i], '/', meta$ptt[i], '-MinMaxDepth.csv', sep=''))

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
fe <- file.exists(paste('~/work/Data/meso/', tolower(meta$speciescode[i]), '/', meta$ptt[i], '/', meta$ptt[i], '-SST.csv', sep=''))

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
fe <- file.exists(paste('~/work/Data/meso/', tolower(meta$speciescode[i]), '/', meta$ptt[i], '/', meta$ptt[i], '-ml.csv', sep=''))

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
fe <- file.exists(paste('~/work/Data/meso/', tolower(meta$speciescode[i]), '/', meta$ptt[i], '/', meta$ptt[i], '-Histos.csv', sep=''))

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


## MIX LAYER?




if (meta$ptt[i] == 100976){
  series$depth[which(series$depth > 1700)] <- NA
  results <- NULL
  pdt <- pdt[which(pdt$depth < 1700),]
  mmd <- mmd[which(mmd$maxDepth < 1700),]
}

#allList[[i]] <- list(track=NA, loess=NA, series=NA, pdt=NA, mmd=NA, sst=NA, histos=NA)
allList[[i]] <- list(track = track, loess = results, series = series, pdt = pdt, mmd = mmd, sst = sst, histos = histos)
rm(track); rm(pdt); rm(series); rm(mmd); rm(sst); rm(histo.new); rm(histos); rm(results); gc()
print(paste(i, 'of', nrow(meta), 'complete.'))
}


