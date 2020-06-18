#' Get header as metadata from eTUFF file
#'
#' WARNING: this is an internal function. Instead see \link{\code{get_header}}.
#'
#' @param etuff_file is file name to etuff file of interest
#' @return a dataframe of metadata from the header of an eTUFF file
#'

get_etuff_hdr <- function(etuff_file){
  x <- scan(etuff_file, what = character(), sep=',', nmax = 1000)

  # figure out how many hdr lines to skip when reading the data
  skipLines <- grep('DateTime', x) - 1

  # trim x to just the header
  x <- x[1:skipLines]
  x <- x[grep('=', x)]

  for (i in 1:length(x)){

    varName <- stringr::str_trim(
      substr(x[i],
             stringr::str_locate(x[i], ':')[2] + 1,
             stringr::str_locate(x[i], '=')[2] - 2))

    varVal <- stringr::str_trim(
      substr(x[i],
             stringr::str_locate(x[i], '= ')[2],
             nchar(x[i])))
    if (i == 1){
      varDF <- data.frame(varName = as.character(varName), varVal = as.character(varVal))
    } else{
      varDF$varName <- as.character(varDF$varName)
      varDF$varVal <- as.character(varDF$varVal)
      varDF[i,] <- c(varName, varVal)
    }
  }

  varDF$varName <- as.character(varDF$varName)
  varDF$varVal <- as.character(varDF$varVal)
  varDF[c(length(x) + 1),] <- c('skip', skipLines)
  varDF
}
