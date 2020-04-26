#' @param etuff
#' @param meta_row
#' @param etuff_file is an etuff text file
#' @param header is logical indicating whether or not the target etuff_file has a header. This will nearly always be TRUE (default).
#' @param metaTypes is a dataframe that describes the appropriate inventory of metadata vocabulary. Default is NULL in which this table is read from Github.

write_etuff <- function(etuff, meta_row = NULL, etuff_file, check_meta = TRUE,...){


  args <- list(...)
  if ('obsTypes' %in% names(args)){
    obsTypes <- args$obsTypes
  } else{
    obsTypes <- NULL
  }

  if (class(etuff) != 'etuff') stop('Input etuff object must be of class etuff.')
  if (is.null(meta_row)) meta_row <- etuff$meta

  ## check meta for issues
  if (check_meta) build_meta_head(meta_row = meta_row, write_hdr = F)

  ## reshape bins
  if (!is.null(etuff$bins)){
    bins <- reshape2::melt(etuff$bins)#, id.vars=c('DateTime'))
    names(bins) <- c('VariableName','VariableValue')
  }

  ## reshape etuff
  etuff <- reshape2::melt(etuff$etuff, id.vars=c('DateTime'))
  names(etuff) <- c('DateTime','VariableName','VariableValue')

  if (class(etuff$DateTime)[1] != 'POSIXct'){
    etuff <- etuff[which(etuff$DateTime != ''),]
    etuff$DateTime <- as.POSIXct(etuff$DateTime, tz='UTC')
  }

  ## prep for and rbind
  if (!is.null(etuff$bins)){
    bins$DateTime <- ''
    bins <- bins[,c('DateTime','VariableName','VariableValue')]
    etuff$DateTime <- format(etuff$DateTime, '%Y-%m-%d %H:%M:%S', tz='UTC')
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

  etuff <- merge(x = etuff, y = obsTypes[ , c('VariableID', 'VariableName', 'VariableUnits')], by = "VariableName", all.x=TRUE)
  etuff <- etuff[,c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')]

  ## write the output
  build_meta_head(meta_row = meta_row, filename = etuff_file, write_hdr = T)
  #write.table(etuff, file = etuff_file, sep = ',', col.names = F, row.names = F, quote = F, append=T)
  data.table::fwrite(etuff, file = etuff_file, sep = ',', col.names = F, row.names = F, quote = F, append=T)

  print(paste('Adding data to eTUFF file ', etuff_file, '.', sep=''))

}
