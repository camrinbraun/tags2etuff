#' @param etuff
#' @param meta_row
#' @param etuff_file is an etuff text file
#' @param header is logical indicating whether or not the target etuff_file has a header. This will nearly always be TRUE (default).
#' @param metaTypes is a dataframe that describes the appropriate inventory of metadata vocabulary. Default is NULL in which this table is read from Github.

write_etuff <- function(etuff, meta_row = NULL, etuff_file, check_meta = TRUE){

  if (class(etuff) != 'etuff') stop('Input etuff object must be of class etuff.')
  if (is.null(meta_row)) meta_row <- etuff$meta

  ## check meta for issues
  if (check_meta) build_meta_head(meta_row = meta_row, write_hdr = F)

  ## reshape bins
  bins <- reshape2::melt(etuff$bins)#, id.vars=c('DateTime'))
  names(bins) <- c('VariableName','VariableValue')

  ## reshape etuff
  etuff <- reshape2::melt(etuff$etuff, id.vars=c('DateTime'))
  names(etuff) <- c('DateTime','VariableName','VariableValue')

  if (class(etuff$DateTime)[1] != 'POSIXct'){
    etuff <- etuff[which(etuff$DateTime != ''),]
    etuff$DateTime <- as.POSIXct(etuff$DateTime, tz='UTC')
  }

  ## prep for and rbind
  bins$DateTime <- ''
  bins <- bins[,c('DateTime','VariableName','VariableValue')]
  etuff$DateTime <- format(etuff$DateTime, '%Y-%m-%d %H:%M:%S', tz='UTC')
  etuff <- rbind(etuff, bins)

  ## write the output
  build_meta_head(meta_row = meta_row, filename = etuff_file, write_hdr = T)
  write.table(etuff, file = etuff_file, sep = ',', col.names = F, row.names = F, quote = F, append=T)

  print(paste('Output eTUFF file written to ', etuff_file, '.', sep=''))

}
