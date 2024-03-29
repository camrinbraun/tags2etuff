#' Write eTUFF output file
#'
#' Write eTUFF output file
#'
#' @param etuff input etuff object
#' @param meta_row optional metadata input but typically the etuff file already has this
#' @param etuff_file is desired output etuff file
#' @param check_meta is logical indicating whether the metadata should be checked prior to writing
#' @param metaTypes optional. is a dataframe that describes the appropriate inventory of metadata vocabulary. Default is NULL in which this table is read from Github.
#' @return writes output etuff to disk
#' @export

write_etuff <- function(etuff, meta_row = NULL, etuff_file, check_meta = TRUE,...){


  args <- list(...)
  if ('obsTypes' %in% names(args)){
    obsTypes <- args$obsTypes
  } else{
    obsTypes <- NULL
  }

  if (class(etuff) != 'etuff' & class(etuff) != 'etuff_archival') stop('Input object must be of class etuff or etuff_archival.')

  if (is.null(meta_row)) meta_row <- etuff$meta

  ## check meta for issues
  if (check_meta) build_meta_head(meta_row = meta_row, write_hdr = F)

  ## reshape bins
  if (!is.null(etuff$bins)){
    bins <- reshape2::melt(etuff$bins, measure.vars = c(1:ncol(etuff$bins)))#, id.vars=c('DateTime'))
    names(bins) <- c('VariableName','VariableValue')
  } else{
    bins <- NULL
  }

  if (class(etuff) == 'etuff'){
    ## reshape etuff
    etuff <- reshape2::melt(etuff$etuff, id.vars=c('DateTime'))
    names(etuff) <- c('DateTime','VariableName','VariableValue')

  } else if (class(etuff) == 'etuff_archival'){
    etuff <- etuff$etuff
  }

  if (class(etuff$DateTime)[1] != 'POSIXct'){
    etuff <- etuff[which(etuff$DateTime != ''),]
    etuff$DateTime <- as.POSIXct(etuff$DateTime, tz='UTC')
  }

  ## prep for and rbind
  if (!is.null(bins)){
    bins$DateTime <- ''
    bins <- bins[,c('DateTime','VariableName','VariableValue')]
    etuff$DateTime <- format(etuff$DateTime, '%Y-%m-%d %H:%M:%S', tz='UTC')
    etuff <- etuff[which(!(etuff$VariableName %in% unique(bins$VariableName))),]
    etuff <- rbind(etuff, bins)
  } else{
    etuff$DateTime <- format(etuff$DateTime, '%Y-%m-%d %H:%M:%S', tz='UTC')
  }

  if (is.null(obsTypes)){
    # try to get it from github
    print('Getting obsTypes...')
    url <- "https://raw.githubusercontent.com/camrinbraun/tagbase/master/eTUFF-ObservationTypes.csv"
    obsTypes <- try(read.csv(text=RCurl::getURL(url)), TRUE)

    # if that doesnt work, kill the funciton
    if (class(obsTypes) == 'try-error') stop(paste('obsTypes not specified in function call and unable to automatically download it from github at', url, sep=' '))

  }
  if (names(obsTypes)[1] != 'VariableID') names(obsTypes)[1] <- 'VariableID'

  etuff <- etuff[which(etuff$VariableName != 'id'),]

  etuff <- base::merge(x = etuff, y = obsTypes[ , c('VariableID', 'VariableName', 'VariableUnits')], by = "VariableName", all.x=TRUE)
  etuff <- etuff[,c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')]
  etuff <- etuff[order(etuff$DateTime),]
  etuff <- etuff[which(!is.na(etuff$VariableValue)),]

  ## drop those where TAD/TAT bin definitions are arbitrarily assigned timestamps
  #etuff <- etuff[-which(etuff$VariableID >= 301 & etuff$VariableID <= 364 & etuff$DateTime != ''),]

  ## write the output
  build_meta_head(meta_row = meta_row, filename = etuff_file, write_hdr = T)
  #write.table(etuff, file = etuff_file, sep = ',', col.names = F, row.names = F, quote = F, append=T)
  data.table::fwrite(etuff, file = etuff_file, sep = ',', col.names = F, row.names = F, quote = F, append=T)

  print(paste('Adding data to eTUFF file ', etuff_file, '.', sep=''))

}
