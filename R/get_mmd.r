#' Get min/max depth data from eTUFF
#'
#' Extract min/max depth data as typically contained in -MinMaxDepth.csv output from Wildlife Computers
#'
#' @param etuff is object of class etuff
#' @return a dataframe of min/max depth information
#' @export
#'

get_mmd <- function(etuff){

  if (class(etuff) != 'etuff' & class(etuff) != 'etuff_archival') stop('Input object must be of class etuff or etuff_archival.')

  meta <- etuff$meta; df <- etuff$etuff

  ## isolate the appropriate data
  if (class(etuff) == 'etuff_archival'){
    df <- archival_to_etuff(df, vars = c('DateTime','depthMin','depthMax'))
  }

  mmd <- df[,c(which(names(df) %in% c('DateTime','depthMin','depthMax')))]
  if (class(mmd) != 'data.frame') return(mmd = NA)
  mmd <- mmd[which(!is.na(mmd$DateTime)),]

  mmd$depthMax <- as.numeric(mmd$depthMax)
  mmd$depthMin <- as.numeric(mmd$depthMin)
  mmd <- mmd[which(!is.na(mmd$depthMin) | !is.na(mmd$depthMax)),]

  return(mmd)
}
