#' @param etuff is a valid etuff object
#tat.plot <- levelplot(tad$freq ~ tad$DateTime * tad$bin_max)

get_tat <- function(etuff){

  if (class(etuff) != 'etuff' & class(etuff) != 'etuff_archival') stop('Input object must be of class etuff or etuff_archival.')

  meta <- etuff$meta; df <- etuff$etuff

  if (class(etuff) == 'etuff_archival'){
    #idx <- grep('timeattemp', df$VariableName, ignore.case = T)
    #if (length(idx) == 0) stop('No names in this eTUFF file correspond to tat data.')

    #tad <- df[idx,]
   # tad <- tad %>% dplyr::select(-c(id)) %>% spread(VariableName, VariableValue)

    tad <- archival_to_etuff(df, vars = c('TimeAtTemp'))

  } else{
    idx <- grep('timeattemp', names(df), ignore.case = T)
    if (length(idx) == 0) stop('No names in this eTUFF file correspond to tad data.')

    tad <- df[,idx]
    tad <- cbind(DateTime = df$DateTime, tad)

  }


  row_idx <- apply(tad, 1, FUN=function(x) all(is.na(x[grep('timeattemp', names(tad), ignore.case = T)])))
  tad <- tad[which(!row_idx),]
  tad$day <- as.Date(tad$DateTime)

  vars = names(tad)[grep('timeattemp', names(tad), ignore.case = TRUE)]

  tat.new <- gather(tad, bin, freq, vars, factor_key=TRUE)
  tat.new$freq <- as.numeric(tat.new$freq)

  #tat.new <- stats::reshape(tad, ids = tad$DateTime, direction = 'long',
  #                      varying = vars, times = vars, sep='')#, timevar = 'BinNum')
  #tad <- tad[,c('DateTime','time','timeattempBin')]

  ubins <- etuff$bins[,grep('histtemp', names(etuff$bins), ignore.case = T)]
  bin_id <- unique(substr(names(ubins), 15, 16))
  tat.new$bin_min <- NA; tat.new$bin_max <- NA
  for (zz in 1:length(bin_id)){
    ## identify min/max for given bin
    ubins.zz <- ubins[grep(bin_id[zz], names(ubins))]
    put_idx <- grep(bin_id[zz], tat.new$bin)
    tat.new$bin_min[put_idx] <- as.numeric(ubins.zz[,grep('min', names(ubins.zz), ignore.case = T)])
    tat.new$bin_max[put_idx] <- as.numeric(ubins.zz[,grep('max', names(ubins.zz), ignore.case = T)])
  }

  tat.new <- tat.new[,-which(names(tat.new) %in% 'day')]
  tat.new <- tat.new[order(tat.new$DateTime, tat.new$bin_min),]
  row.names(tat.new) <- NULL

  return(tat.new)

}
