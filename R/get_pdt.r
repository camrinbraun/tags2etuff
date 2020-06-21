#' Get depth-temperature profile data from eTUFF
#'
#' Extract depth-temperature profile data as typically contained in -PDTs.csv output from Wildlife Computers
#'
#' @param etuff is object of class etuff
#' @return a dataframe of min/max depth information
#' @export
#'

get_pdt <- function(etuff){

  if (class(etuff) != 'etuff' & class(etuff) != 'etuff_archival') stop('Input object must be of class etuff or etuff_archival.')

  meta <- etuff$meta; df <- etuff$etuff

  ## isolate the appropriate data
  if (class(etuff) == 'etuff_archival'){
    pdt <- archival_to_etuff(df, vars = c('pdt'))
  } else{
    idx <- grep('pdt', names(df), ignore.case = T)
    if (length(idx) == 0) stop('No names in this eTUFF file correspond to PDT data.')
    pdt <- df[,idx]
    pdt <- cbind(DateTime = df$DateTime, pdt)
  }

  pdt <- pdt[which(!is.na(pdt$DateTime) & !is.na(pdt$PdtDepth01) & pdt$PdtDepth01 != ''),]
  pdt$day <- as.Date(pdt$DateTime)

  vars = names(pdt)[grep('pdt', names(pdt), ignore.case = TRUE)]

  pdt <- stats::reshape(pdt, ids = pdt$day, direction = 'long',
                        varying = vars, times = vars, sep='')#, timevar = 'BinNum')

  pdt <- pdt[,c('DateTime','PdtDepth','PdtTempMax','PdtTempMin')]
  row.names(pdt) <- NULL
  pdt$PdtDepth <- as.numeric(pdt$PdtDepth)
  pdt$PdtTempMax <- as.numeric(pdt$PdtTempMax)
  pdt$PdtTempMin <- as.numeric(pdt$PdtTempMin)
  pdt <- pdt[which(!is.na(pdt$PdtDepth)),]
  pdt <- pdt[order(pdt$DateTime, pdt$PdtDepth),]

  return(pdt)

}
