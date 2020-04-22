# directories stored somewhere as taxonomicSerial_tagYear_ptt. ready to be read.

taxNum <- unique(meta$taxonomic_serial_number)

for (i in taxNum){
  meta.i <- meta[which(meta$taxonomic_serial_number == i),]
  tgYr <- lubridate::year(as.POSIXct(meta.i$time_coverage_start, format='%m/%d/%y'))

  for (b in unique(tgYr)){
    select.ptts <- meta.i$ptt[which(tgYr == b)]

    for (t in select.ptts){
      dir.create(paste(i, '_', b, '_', t, sep=''))

    }

  }
  invisible(readline(prompt=paste('Finished ', meta.i$platform[1],' - ', meta.i$taxonomic_serial_number[1], '. Press [enter] to continue.', sep='')))

}


# brad's makos in OC_makos_for_camrin.xls and tigers in Tigers for Camrin.xls (see read_brad_data.r)
# update any existing spot data with ocearch-derived data


#********
#> ## getting going with brads mako tiger data to etuff
#> ## should prob just include timestamp, lat/lon, location class?

dirlist <- list.dirs('~/Desktop/data_org/')
dirlist <- dirlist[sapply(dirlist, function(x) length(list.files(x)) == 0)]


#------------------
## now for brad tiger and mako data
#------------------

load('~/work/RCode/eddies/data/all_brad_combined.rda')
df <- df[which(df$class != 'G'),]

world <- map_data('world')
meta$geospatial_lat_start <- as.numeric(as.character(meta$geospatial_lat_start))
meta$geospatial_lon_start <- as.numeric(as.character(meta$geospatial_lon_start))
meta$geospatial_lat_end <- as.numeric(as.character(meta$geospatial_lat_end))
meta$geospatial_lon_end <- as.numeric(as.character(meta$geospatial_lon_end))


u.ptts <- unique(df$ptt)
for (i in 66:length(u.ptts)){
  #argos <- read.table(paste(dir, meta$ptt[i], '-Locations.csv', sep=''), sep=',', header=T, blank.lines.skip = F)
  argos <- df[which(df$ptt == u.ptts[i]),]

  cutdates <- read.table('~/work/RCode/eddies/raw/cut_dates.csv', sep=',',header=T)
  cutdates$rm_date1 <- parse_date_time(cutdates$rm_date1, orders = 'mdy HM')
  cutdates$rm_date2 <- parse_date_time(cutdates$rm_date2, orders = 'mdy HM')

  if(any(argos$ptt[1] %in% cutdates$ptt)){
    cut.idx <- which(cutdates$ptt %in% argos$ptt[1])
    for (b in 1:length(cut.idx)){
      argos <- argos[which(argos$dt < cutdates$rm_date1[cut.idx[b]] | argos$dt > cutdates$rm_date2[cut.idx[b]]),]
    }
  }

  # get meta
  idx <- which(meta$ptt == u.ptts[i])
  if (length(idx) > 1) stop('idx is > 1. check meta.')
  iniloc <- data.frame(matrix(c(day(meta$time_coverage_start[idx]), month(meta$time_coverage_start[idx]), year(meta$time_coverage_start[idx]), meta$geospatial_lat_start[idx], meta$geospatial_lon_start[idx],
                                day(meta$time_coverage_end[idx]), month(meta$time_coverage_end[idx]), year(meta$time_coverage_end[idx]), meta$geospatial_lat_end[idx], meta$geospatial_lon_end[idx]), nrow = 2, ncol = 5, byrow = T))
  colnames(iniloc) = list('day','month','year','lat','lon')
  tag <- as.POSIXct(paste(iniloc[1,1], '/', iniloc[1,2], '/', iniloc[1,3], sep=''), format = '%d/%m/%Y', tz='UTC')
  pop <- as.POSIXct(paste(iniloc[2,1], '/', iniloc[2,2], '/', iniloc[2,3], sep=''), format = '%d/%m/%Y', tz='UTC')
  if (is.na(pop)) pop <- Sys.Date()
  iniloc$DateTime <- c(tag, pop)


  # organize argos.new for flatfile format
  #argos.new <- argos[which(argos$Type != 'User'),]
  nms <- tolower(names(argos))
  nms[grep('class', nms)] <- 'argosLC'
  nms[grep('lat', nms)] <- 'latitude'
  nms[grep('lon', nms)] <- 'longitude'
  names(argos) <- nms
  #argos$dt <- as.POSIXct(argos$dt, format='%H:%M:%S %d-%b-%Y', tz='UTC')
  argos <- argos[which(argos$dt > tag & argos$dt < pop),]
  argos <- argos %>% distinct(dt, .keep_all = T)

  argos <- reshape2::melt(argos, id.vars=c('dt','ptt'), measure.vars = c('argosLC','latitude','longitude'))
  argos$VariableName <- argos$variable

  argos <- merge(x = argos, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
  argos <- argos[,c('ptt','dt','VariableID','value','VariableName','VariableUnits')]
  names(argos) <- c('ptt','DateTime','VariableID','VariableValue','VariableName','VariableUnits')
  argos <- argos[order(argos$DateTime, argos$VariableID),]
  argos$DateTime <- as.POSIXct(argos$DateTime, tz='UTC')

  argos.try <- argos %>% select(-c(VariableID, VariableUnits)) %>% spread(VariableName, VariableValue)
  argos.try$latitude <- as.numeric(argos.try$latitude)
  argos.try$longitude <- as.numeric(argos.try$longitude)
  xl <- c(min(argos.try$longitude), max(argos.try$longitude))
  yl <- c(min(argos.try$latitude), max(argos.try$latitude))

  if (pop == Sys.Date()){
    iniloc[2,1:5] <- c(lubridate::day(argos.try$DateTime[nrow(argos.try)]),
                       lubridate::month(argos.try$DateTime[nrow(argos.try)]),
                       lubridate::year(argos.try$DateTime[nrow(argos.try)]),
                       argos.try$latitude[nrow(argos.try)],
                       argos.try$longitude[nrow(argos.try)])
    iniloc[2,6] <- argos.try$DateTime[nrow(argos.try)]
  }

  pdf(paste(u.ptts[i], '-ggmap.pdf', sep=''), width=8, height=12)
  p1 <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) +
    coord_fixed(xlim=xl, ylim=yl, ratio=1.3) + xlab('') + ylab('') +
    geom_path(data = argos.try, aes(x = longitude, y = latitude)) +
    geom_point(data = argos.try, aes(x = longitude, y = latitude, colour = DateTime)) +
    geom_point(data = iniloc, aes(x = lon, y = lat), colour = c('green','red'), fill = c('green','red'), shape = 24) +
    ggtitle(paste(u.ptts[i]))

  p2 <- ggplot(argos.try, aes(y = latitude, x = DateTime, colour = DateTime)) + geom_point() + geom_path() +
    geom_point(data = iniloc, aes(x = DateTime, y = lat), colour = c('green','red'), fill = c('green','red'), shape = 24)

  p3 <- ggplot(argos.try, aes(y = longitude, x = DateTime, colour = DateTime)) + geom_point() + geom_path() +
    geom_point(data = iniloc, aes(x = DateTime, y = lon), colour = c('green','red'), fill = c('green','red'), shape = 24)

  grid.arrange(grobs = list(p1, p2, p3), ncol=1, heights = c(6,2,2))
               #layout_matrix = rbind(c(1), c(2), c(3)))
  dev.off()

  print(round(difftime(argos.try$DateTime[nrow(argos.try)], iniloc$DateTime[1], 'days'),0))

  invisible(readline(prompt=paste('Check plot for ', u.ptts[i],'(i=',i,'). Press [enter] to continue.', sep='')))

  if (pop == Sys.Date()){
    ## put derived end point into meta
    meta$geospatial_lon_end[idx] <- iniloc$lon[2]
    meta$geospatial_lat_end[idx] <- iniloc$lat[2]
    meta$time_coverage_end[idx] <- iniloc$DateTime[2]
    meta$end_details <- 'final SPOT'
  }

  #argos$DateTime <- format(argos$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
  if (exists('returnData')){
    returnData <- rbind(returnData, argos)
  } else {
    returnData <- argos
  }
}

write.table(meta, file='~/work/RCode/eddies/all_tag_meta_v3.csv', sep=',', row.names = F, col.names = T)
save(returnData, file='~/work/RCode/eddies/raw/brad_argos_etuff_raw.rda')

#------------------
## now for Kock white shark data
#------------------

dat <- read.table('~/work/Data/eddies/Kock/SPOT-Locations.csv', sep=',', header=T)
dat <- dat[,c('Ptt','Date','Quality','Latitude','Longitude')]
dat$Date <- as.POSIXct(dat$Date, format='%H:%M:%S %d-%b-%Y', tz='UTC')
colnames(dat) <- c('ptt','dt','class','lat','lon')

df <- dat[which(dat$class != ' ' & dat$class != 'Z'),]

#world <- map_data('world')
meta$geospatial_lat_start <- as.numeric(as.character(meta$geospatial_lat_start))
meta$geospatial_lon_start <- as.numeric(as.character(meta$geospatial_lon_start))
meta$geospatial_lat_end <- as.numeric(as.character(meta$geospatial_lat_end))
meta$geospatial_lon_end <- as.numeric(as.character(meta$geospatial_lon_end))


u.ptts <- unique(df$ptt)
for (i in 33:length(u.ptts)){
  #argos <- read.table(paste(dir, meta$ptt[i], '-Locations.csv', sep=''), sep=',', header=T, blank.lines.skip = F)
  argos <- df[which(df$ptt == u.ptts[i]),]

  cutdates <- read.table('~/work/RCode/eddies/raw/cut_dates.csv', sep=',',header=T)
  cutdates$rm_date1 <- parse_date_time(cutdates$rm_date1, orders = 'mdy HM')
  cutdates$rm_date2 <- parse_date_time(cutdates$rm_date2, orders = 'mdy HM')

  if(any(argos$ptt[1] %in% cutdates$ptt)){
    cut.idx <- which(cutdates$ptt %in% argos$ptt[1])
    for (b in 1:length(cut.idx)){
      argos <- argos[which(argos$dt < cutdates$rm_date1[cut.idx[b]] | argos$dt > cutdates$rm_date2[cut.idx[b]]),]
    }
  }

  # get meta
  idx <- which(meta$ptt == u.ptts[i])
  if (length(idx) > 1) stop('idx is > 1. check meta.')
  iniloc <- data.frame(matrix(c(day(meta$time_coverage_start[idx]), month(meta$time_coverage_start[idx]), year(meta$time_coverage_start[idx]), meta$geospatial_lat_start[idx], meta$geospatial_lon_start[idx],
                                day(meta$time_coverage_end[idx]), month(meta$time_coverage_end[idx]), year(meta$time_coverage_end[idx]), meta$geospatial_lat_end[idx], meta$geospatial_lon_end[idx]), nrow = 2, ncol = 5, byrow = T))
  colnames(iniloc) = list('day','month','year','lat','lon')
  tag <- as.POSIXct(paste(iniloc[1,1], '/', iniloc[1,2], '/', iniloc[1,3], sep=''), format = '%d/%m/%Y', tz='UTC')
  pop <- as.POSIXct(paste(iniloc[2,1], '/', iniloc[2,2], '/', iniloc[2,3], sep=''), format = '%d/%m/%Y', tz='UTC')
  if (is.na(pop)) pop <- Sys.Date()
  iniloc$DateTime <- c(tag, pop)

  # organize argos.new for flatfile format
  #argos.new <- argos[which(argos$Type != 'User'),]
  nms <- tolower(names(argos))
  nms[grep('class', nms)] <- 'argosLC'
  nms[grep('lat', nms)] <- 'latitude'
  nms[grep('lon', nms)] <- 'longitude'
  names(argos) <- nms
  #argos$dt <- as.POSIXct(argos$dt, format='%H:%M:%S %d-%b-%Y', tz='UTC')
  argos <- argos[which(argos$dt > tag & argos$dt < pop),]
  argos <- argos %>% distinct(dt, .keep_all = T)

  argos <- reshape2::melt(argos, id.vars=c('dt','ptt'), measure.vars = c('argosLC','latitude','longitude'))
  argos$VariableName <- argos$variable

  argos <- merge(x = argos, y = obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
  argos <- argos[,c('ptt','dt','VariableID','value','VariableName','VariableUnits')]
  names(argos) <- c('ptt','DateTime','VariableID','VariableValue','VariableName','VariableUnits')
  argos <- argos[order(argos$DateTime, argos$VariableID),]
  argos$DateTime <- as.POSIXct(argos$DateTime, tz='UTC')

  argos.try <- argos %>% select(-c(VariableID, VariableUnits)) %>% spread(VariableName, VariableValue)
  argos.try$latitude <- as.numeric(argos.try$latitude)
  argos.try$longitude <- as.numeric(argos.try$longitude)
  xl <- c(min(argos.try$longitude), max(argos.try$longitude))
  yl <- c(min(argos.try$latitude), max(argos.try$latitude))

  if (pop == Sys.Date()){
    iniloc[2,1:5] <- c(lubridate::day(argos.try$DateTime[nrow(argos.try)]),
                       lubridate::month(argos.try$DateTime[nrow(argos.try)]),
                       lubridate::year(argos.try$DateTime[nrow(argos.try)]),
                       argos.try$latitude[nrow(argos.try)],
                       argos.try$longitude[nrow(argos.try)])
    iniloc[2,6] <- argos.try$DateTime[nrow(argos.try)]
  }

  pdf(paste(u.ptts[i], '-ggmap.pdf', sep=''), width=8, height=12)
  p1 <- ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group)) +
    coord_fixed(xlim=xl, ylim=yl, ratio=1.3) + xlab('') + ylab('') +
    geom_path(data = argos.try, aes(x = longitude, y = latitude)) +
    geom_point(data = argos.try, aes(x = longitude, y = latitude, colour = DateTime)) +
    geom_point(data = iniloc, aes(x = lon, y = lat), colour = c('green','red'), fill = c('green','red'), shape = 24) +
    ggtitle(paste(u.ptts[i]))

  p2 <- ggplot(argos.try, aes(y = latitude, x = DateTime, colour = DateTime)) + geom_point() + geom_path() +
    geom_point(data = iniloc, aes(x = DateTime, y = lat), colour = c('green','red'), fill = c('green','red'), shape = 24)

  p3 <- ggplot(argos.try, aes(y = longitude, x = DateTime, colour = DateTime)) + geom_point() + geom_path() +
    geom_point(data = iniloc, aes(x = DateTime, y = lon), colour = c('green','red'), fill = c('green','red'), shape = 24)

  grid.arrange(grobs = list(p1, p2, p3), ncol=1, heights = c(6,2,2))
  #layout_matrix = rbind(c(1), c(2), c(3)))
  dev.off()

  print(round(difftime(argos.try$DateTime[nrow(argos.try)], iniloc$DateTime[1], 'days'),0))

  invisible(readline(prompt=paste('Check plot for ', u.ptts[i],'(i=',i,'). Press [enter] to continue.', sep='')))

  if (pop == Sys.Date()){
    ## put derived end point into meta
    meta$geospatial_lon_end[idx] <- iniloc$lon[2]
    meta$geospatial_lat_end[idx] <- iniloc$lat[2]
    meta$time_coverage_end[idx] <- iniloc$DateTime[2]
    meta$end_details <- 'final SPOT'
  }

  #argos$DateTime <- format(argos$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
  if (exists('returnData')){
    returnData <- rbind(returnData, argos)
  } else {
    returnData <- argos
  }
}

#meta$geospatial_lat_start[idx] <- meta$geospatial_lat_start[idx] * -1
#meta$geospatial_lat_start[which(meta$person_owner == 'Alison Kock' & meta$geospatial_lat_start > 0)] <- meta$geospatial_lat_start[which(meta$person_owner == 'Alison Kock' & meta$geospatial_lat_start > 0)] * -1

write.table(meta, file='~/work/RCode/eddies/all_tag_meta_v4.csv', sep=',', row.names = F, col.names = T)
save(returnData, file='~/work/RCode/eddies/raw/kock_argos_etuff_raw.rda')


## Cleaning of SPOT positions should include:
# - deal with gaps as splitting into separate trajectories
# - speed filter adn regularize temporal resolution


## changes to ObservationTypes
# - comment that WC often has mixed layer metrics although perhaps not worth adding to obs types?
# - how to deal with WC light data?

## after formulating flatfile:
# check for rows that are completely duplicated (with or without the same VariableValue)
# check for outliers in variablevalue
# cut based on start/end timestamps


## left to build in:
# do recovered WC tags negate the need for most other metrics? or in addition to?
#  LightLoc

# give the function a row of metadata and a directory
## the function gets necessary info from meta like tag type, start/end, etc.
## then runs appropriate parse functions on the necessary files in the directory

# get meta types
metaTypes <- read.csv(url("https://raw.githubusercontent.com/camrinbraun/tagbase/master/eTagMetadataInventory.csv"))
metaTypes$Necessity[which(metaTypes$AttributeID %in% c(3,8,100,101,200,302,400:404,1000))] <- 'recommended'

# get observation types
#tFile <- tempfile()
#curl::curl_download(url="https://github.com/tagbase/tagbase/blob/master/eTUFF-ObservationTypes.xlsx?raw=true", destfile = tFile)
#curl::curl_download(url="https://raw.githubusercontent.com/camrinbraun/tagbase/master/eTUFF-ObservationTypes.csv", destfile = tFile)
#obsTypes <- gdata::read.xls(tFile, sheet='ObservationTypes')
#obsTypes <- read.table(tFile, sep=',', header=T, blank.lines.skip=F, colClasses=c('character','character', rep('NULL', 6)), quote="", fill=F)

obsTypes <- read.csv(text=RCurl::getURL("https://raw.githubusercontent.com/camrinbraun/tagbase/master/eTUFF-ObservationTypes.csv"))

# load meta
meta <- read.table('~/work/RCode/eddies/all_tag_meta_v2.csv', sep=',', header=T, blank.lines.skip = F, skip=0)

## get these from meta header...
# TAG/POPUP DATES AND LOCATIONS (dd, mm, YYYY, lat, lon)
library(lubridate)

iniloc <- data.frame(matrix(c(day(meta$time_coverage_start[i]), month(meta$time_coverage_start[i]), year(meta$time_coverage_start[i]), meta$geospatial_lat_start[i], meta$geospatial_lon_start[i],
                              day(meta$time_coverage_end[i]), month(meta$time_coverage_end[i]), year(meta$time_coverage_end[i]), meta$geospatial_lat_end[i], meta$geospatial_lon_end[i]), nrow = 2, ncol = 5, byrow = T))
colnames(iniloc) = list('day','month','year','lat','lon')
tag <- as.POSIXct(paste(iniloc[1,1], '/', iniloc[1,2], '/', iniloc[1,3], sep=''), format = '%d/%m/%Y', tz='UTC')
pop <- as.POSIXct(paste(iniloc[2,1], '/', iniloc[2,2], '/', iniloc[2,3], sep=''), format = '%d/%m/%Y', tz='UTC')
# VECTOR OF DATES FROM DATA. THIS WILL BE THE TIME STEPS, T, IN THE LIKELIHOODS
#dateVec <- as.Date(seq(tag, pop, by = 'day'))


tadBins.i <- tadBins[which(tadBins$SpeciesCode %in% meta$speciescode[i] & tadBins$PTT %in% meta$ptt[i]),]
tatBins.i <- tatBins[which(tatBins$SpeciesCode %in% meta$speciescode[i] & tatBins$PTT %in% meta$ptt[i]),]


tag_to_etuff(dir = dir, manufacturer = 'Wildlife', tagtype = 'PSAT', dates=dates, tatBins = NULL, tadBins = NULL)


## need some interactive cleaning steps!!!!
# build a QC function, partial output of which does the meta QC portion and denotes a meta column that tag is ready for ingestion



# use meta function to build the header
build_meta_head(meta_row = meta[51,], filename = paste('eTUFF_', meta$platform[51], '_', meta$ptt[51], '.txt', sep=''))




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



library(DBI)
con <- DBI::dbConnect(RPostgres::Postgres(), dbname='postgres', host='0.0.0.0', port=5432, user='tagbase', password='tagbase')

con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), dbname = "postgres",
                      host = "0.0.0.0", port = 5432,
                      user = "tagbase", password = "tagbase")

