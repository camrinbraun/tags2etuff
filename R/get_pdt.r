#' @param etuff is a valid etuff object

get_pdt <- function(etuff){

  if (class(etuff) != 'etuff') stop('Input object must be of class etuff.')

  meta <- etuff$meta; df <- etuff$etuff

  idx <- grep('pdt', names(df), ignore.case = T)
  if (length(idx) == 0) stop('No names in this eTUFF file correspond to PDT data.')

  pdt <- df[,idx]
  pdt <- cbind(datetime = df$datetime, pdt)
  pdt <- pdt[which(!is.na(pdt$datetime) & !is.na(pdt$PdtDepth01)),]
  pdt$day <- as.Date(pdt$datetime)

  vars = names(pdt)[grep('pdt', names(pdt), ignore.case = TRUE)]

  pdt <- stats::reshape(pdt, ids = pdt$day, direction = 'long',
                        varying = vars, times = vars, sep='')#, timevar = 'BinNum')

  pdt <- pdt[,c('datetime','PdtDepth','PdtTempMax','PdtTempMin')]
  row.names(pdt) <- NULL


  pdt <- pdt[order(pdt$datetime, pdt$PdtDepth),]

  return(pdt)

}
