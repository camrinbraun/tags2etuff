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

  if ('qc_obsTypes' %in% names(args)){
    qc_obsTypes <- args$qc_obsTypes
  } else{
    qc_obsTypes <- NULL
  }

  if (class(etuff) != 'etuff') stop('Input etuff object must be of class etuff.')
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

  ## reshape qc
  if ('qc' %in% names(etuff)){
    qc <- etuff$qc %>% select(-id) %>% reshape2::melt(id.vars=c('DateTime'))
    names(qc) <- c('DateTime','VariableName','VariableValue')
    qc <- qc %>% filter(!is.na(VariableValue))

    if (class(qc$DateTime)[1] != 'POSIXct'){
      qc <- qc[which(qc$DateTime != ''),]
      qc$DateTime <- as.POSIXct(qc$DateTime, tz='UTC')
    }
  }

  ## reshape etuff
  etuff <- reshape2::melt(etuff$etuff, id.vars=c('DateTime'))
  names(etuff) <- c('DateTime','VariableName','VariableValue')

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

  etuff <- etuff[which(etuff$VariableName != 'id'),]
  etuff <- merge(x = etuff, y = obsTypes[ , c('VariableID', 'VariableName', 'VariableUnits')], by = "VariableName", all.x=TRUE)
  etuff <- etuff[,c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')]

  if (exists('qc')){
    if (is.null(qc_obsTypes)){
      # try to get it from github
      print('Getting qc_obsTypes...')
      url <- "https://raw.githubusercontent.com/camrinbraun/tagbase/add-qc/eTUFF-qc-ObservationTypes.csv"
      qc_obsTypes <- try(read.csv(text=RCurl::getURL(url)), TRUE)
      # if that doesnt work, kill the funciton
      if (class(qc_obsTypes) == 'try-error') stop(paste('qc_obsTypes not specified in function call and unable to automatically download it from github at', url, sep=' '))
    }

    qc <- merge(x = qc, y = qc_obsTypes[ , c("VariableID","VariableName", 'VariableUnits')], by = "VariableName", all.x=TRUE)
    qc <- qc[,c('DateTime','VariableID','VariableValue','VariableName','VariableUnits')]
    qc <- qc[order(qc$DateTime, qc$VariableID),]
    qc$DateTime <- as.POSIXct(qc$DateTime, tz='UTC')
    qc$DateTime <- format(qc$DateTime, '%Y-%m-%d %H:%M:%S') # yyyy-mm-dd hh:mm:ss
    qc <- qc %>% filter(!is.na(VariableValue))

    etuff <- rbind(etuff, qc)
  }

  ## drop those where TAD/TAT bin definitions are arbitrarily assigned timestamps
  #etuff <- etuff[-which(etuff$VariableID >= 301 & etuff$VariableID <= 364 & etuff$DateTime != ''),]

  ## write the output
  build_meta_head(meta_row = meta_row, filename = etuff_file, write_hdr = T)
  #write.table(etuff, file = etuff_file, sep = ',', col.names = F, row.names = F, quote = F, append=T)
  data.table::fwrite(etuff, file = etuff_file, sep = ',', col.names = F, row.names = F, quote = F, append=T)

  print(paste('Adding data to eTUFF file ', etuff_file, '.', sep=''))

}
