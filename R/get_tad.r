#' Get time-at-depth data from eTUFF
#'
#' Extract time-at-depth data as typically contained in -Histos.csv output from Wildlife Computers
#'
#' @param etuff is object of class etuff
#' @return a dataframe of min/max depth information
#' @export
#'

get_tad <- function(etuff){

  if (class(etuff) != 'etuff' & class(etuff) != 'etuff_archival') stop('Input object must be of class etuff or etuff_archival.')

  meta <- etuff$meta; df <- etuff$etuff


  if (class(etuff) == 'etuff_archival'){
    #idx <- grep('timeatdepth', df$VariableName, ignore.case = T)
    #if (length(idx) == 0) stop('No names in this eTUFF file correspond to tat data.')

    #tad <- df[idx,]
    #tad <- tad %>% dplyr::select(-c(id)) %>% spread(VariableName, VariableValue)

    tad <- archival_to_etuff(df, vars = c('timeatdepth'))

  } else{
    idx <- grep('timeatdepth', names(df), ignore.case = T)
    if (length(idx) == 0) stop('No names in this eTUFF file correspond to tad data.')

    tad <- df[,idx]
    tad <- cbind(DateTime = df$DateTime, tad)

  }

  row_idx <- apply(tad, 1, FUN=function(x) all(is.na(x[grep('timeatdepth', names(tad), ignore.case = T)])))
  tad <- tad[which(!row_idx),]
  tad$day <- as.Date(tad$DateTime)

  vars = names(tad)[grep('timeatdepth', names(tad), ignore.case = TRUE)]

  tad.new <- tidyr::gather(tad, bin, freq, vars, factor_key=TRUE)
  tad.new$freq <- as.numeric(tad.new$freq)

  #tad.new <- stats::reshape(tad, ids = tad$DateTime, direction = 'long',
  #                      varying = vars, times = vars, sep='')#, timevar = 'BinNum')
  #tad <- tad[,c('DateTime','time','TimeAtDepthBin')]

  ubins <- etuff$bins[,grep('histdepth', names(etuff$bins), ignore.case = T)]
  bin_id <- unique(substr(names(ubins), 16, 17))
  for (zz in 1:length(bin_id)){
    ## identify min/max for given bin
    ubins.zz <- ubins[grep(bin_id[zz], names(ubins))]
    put_idx <- grep(bin_id[zz], tad.new$bin)
    tad.new$bin_min[put_idx] <- as.numeric(ubins.zz[,grep('min', names(ubins.zz), ignore.case = T)])
    tad.new$bin_max[put_idx] <- as.numeric(ubins.zz[,grep('max', names(ubins.zz), ignore.case = T)])
  }

  tad.new <- tad.new[,-which(names(tad.new) %in% 'day')]
  tad.new <- tad.new[order(tad.new$DateTime, tad.new$bin_min),]
  row.names(tad.new) <- NULL

  return(tad.new)

}
